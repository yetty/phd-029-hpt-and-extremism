import hashlib
import importlib.util
import io
import json
import os
import tempfile
import unittest
from contextlib import redirect_stderr
from unittest import mock
import zipfile
from pathlib import Path


MODULE = Path(__file__).parents[1] / "tools" / "zenodo_release.py"
SPEC = importlib.util.spec_from_file_location("zenodo_release", MODULE)
zr = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(zr)


class ReleaseToolTests(unittest.TestCase):
    def write_package(self, root, extra=False, denied=False):
        package = root / "osf_storage"
        package.mkdir()
        (package / "README.md").write_text("release\n", encoding="utf-8")
        if extra:
            (package / "unexpected.txt").write_text("x\n", encoding="utf-8")
        if denied:
            (package / "build.log").write_text("x\n", encoding="utf-8")
        metadata = {
            "release": {
                "archive_name": "release.zip",
                "archive_root": "replication_package",
                "state_file": ".zenodo-release/state.json",
                "release_doi": "",
                "version": "1.0",
            },
            "metadata": {
                "title": "A release",
                "description": "A release description.",
                "creators": [{"person_or_org": {
                    "type": "personal", "given_name": "Juda",
                    "family_name": "Kaleta"}}],
                "subjects": [{"subject": "test"}],
                "resource_type": {"id": "dataset"},
                "related_identifiers": [],
                "rights": [{"id": "cc-by-4.0"}],
                "publisher": "Zenodo",
            },
            "allowlist": ["README.md"],
            "denylist": [
                "instruments/**",
                "**/*.log",
                "**/.zenodo-release-state.json",
            ],
        }
        return package, metadata

    def test_collect_sources_requires_exact_allowlist(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            package, metadata = self.write_package(root, extra=True)
            with self.assertRaisesRegex(zr.ReleaseError, "unallowlisted"):
                zr.collect_sources(package, metadata)

    def test_collect_sources_excludes_denied_build_intermediates(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            package, metadata = self.write_package(root, denied=True)
            self.assertEqual(
                zr.collect_sources(package, metadata),
                [package / "README.md"],
            )

    def test_collect_sources_rejects_allowlisted_instrument(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            package, metadata = self.write_package(root)
            instrument = package / "instruments" / "questionnaire.docx"
            instrument.parent.mkdir()
            instrument.write_bytes(b"third-party instrument")
            metadata["allowlist"].append("instruments/questionnaire.docx")
            with self.assertRaisesRegex(zr.ReleaseError, "denylist"):
                zr.collect_sources(package, metadata)

    def test_build_and_verify_bundle_hashes_every_payload_file(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            package, metadata = self.write_package(root)
            output = root / ".zenodo-release"
            archive, manifest = zr.build_bundle(package, metadata, output)
            self.assertTrue(archive.is_file())
            self.assertTrue(manifest.is_file())
            zr.verify_bundle(archive, manifest, metadata)
            lines = manifest.read_text(encoding="utf-8").splitlines()
            self.assertEqual(len(lines), 1)
            self.assertEqual(lines[0].split("  ")[1],
                             "replication_package/README.md")
            with zipfile.ZipFile(archive) as bundle:
                self.assertEqual(bundle.namelist(), [
                    "replication_package/README.md",
                    "MANIFEST-sha256.txt",
                ])

    def test_metadata_requires_an_explicit_rights_decision(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            package, metadata = self.write_package(root)
            del metadata["metadata"]["rights"]
            with self.assertRaisesRegex(zr.ReleaseError, "rights"):
                zr.validate_metadata(metadata)

    def test_metadata_requires_publisher_for_doi_registration(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            package, metadata = self.write_package(root)
            del metadata["metadata"]["publisher"]
            with self.assertRaisesRegex(zr.ReleaseError, "publisher"):
                zr.validate_metadata(metadata)

    def test_api_payload_adds_required_publication_date(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            _, metadata = self.write_package(root)
            payload = zr.metadata_payload(metadata)
            self.assertRegex(payload["publication_date"],
                             r"^\d{4}-\d{2}-\d{2}$")

    def test_production_token_is_required(self):
        with mock.patch.dict(os.environ, {
            "ZENODO_API_KEY": "production-token",
        }, clear=True):
            self.assertEqual(zr.load_token("production", None),
                             "production-token")
        with mock.patch.dict(os.environ, {}, clear=True):
            with self.assertRaisesRegex(zr.ReleaseError, "ZENODO_API_KEY"):
                zr.load_token("production", None)

    def test_only_production_environment_is_supported(self):
        with redirect_stderr(io.StringIO()):
            with self.assertRaises(SystemExit):
                zr.parser().parse_args([
                    "verify", "--environment", "test",
                ])

    def test_api_checksum_requires_zenodo_md5_match(self):
        content = b"release content"
        expected = hashlib.md5(content).hexdigest()
        zr.verify_remote_file({"status": "completed", "size": len(content),
                               "checksum": "md5:" + expected}, content)
        with self.assertRaisesRegex(zr.ReleaseError, "MD5"):
            zr.verify_remote_file({"status": "completed", "size": len(content),
                                    "checksum": "md5:00000000000000000000000000000000"},
                                   content)

    def test_list_files_uses_draft_and_published_file_endpoints(self):
        client = zr.ZenodoClient("production", "not-a-real-token")
        client.request = mock.Mock(return_value=(200, {"entries": []}))
        self.assertEqual(client.list_files("123", published=False), [])
        client.request.assert_called_once_with(
            "GET", "/records/123/draft/files"
        )
        client.request.reset_mock()
        self.assertEqual(client.list_files("123", published=True), [])
        client.request.assert_called_once_with(
            "GET", "/records/123/files", accept="application/json"
        )

    def test_write_state_rejects_secret_fields(self):
        with tempfile.TemporaryDirectory() as directory:
            state_path = Path(directory) / "state.json"
            with self.assertRaisesRegex(zr.ReleaseError, "forbidden field"):
                zr.write_state(state_path, {
                    "environment": "production",
                    "record_id": "123",
                    "token": "must-not-be-written",
                })
            self.assertFalse(state_path.exists())

    def test_failed_doi_reservation_preserves_draft_identity(self):
        class FailingClient:
            def create_draft(self, metadata):
                return {
                    "id": "123",
                    "links": {"self_html": "https://example.test/draft/123"},
                }

            def reserve_doi(self, record_id):
                raise zr.ReleaseError("DOI reservation failed")

        with tempfile.TemporaryDirectory() as directory:
            state_path = Path(directory) / "state.json"
            with self.assertRaisesRegex(zr.ReleaseError, "DOI reservation"):
                zr.create_draft_and_reserve(
                    FailingClient(), "production", {}, state_path
                )
            state = json.loads(state_path.read_text(encoding="utf-8"))
            self.assertEqual(state, {
                "environment": "production",
                "record_id": "123",
                "draft_url": "https://example.test/draft/123",
            })

    def test_publish_needs_both_confirmation_flags(self):
        with self.assertRaisesRegex(zr.ReleaseError, "confirm-publish"):
            zr.require_publish_confirmation(False, True)
        with self.assertRaisesRegex(zr.ReleaseError, "user-approved"):
            zr.require_publish_confirmation(True, False)


if __name__ == "__main__":
    unittest.main(verbosity=2)
