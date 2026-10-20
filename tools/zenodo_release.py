#!/usr/bin/env python3
import argparse
import fnmatch
import hashlib
import json
import os
import sys
import urllib.error
import urllib.request
import zipfile
from datetime import date
from pathlib import Path


API_BASES = {
    "production": "https://zenodo.org/api",
    "sandbox": "https://sandbox.zenodo.org/api",
}


class ReleaseError(RuntimeError):
    pass


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def md5_bytes(content):
    return hashlib.md5(content).hexdigest()


def read_metadata(path):
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ReleaseError("cannot read metadata: " + str(error)) from error


def validate_metadata(document):
    metadata = document.get("metadata", {})
    release = document.get("release", {})
    required = ("title", "description", "creators", "subjects",
                "resource_type", "rights")
    missing = [key for key in required if not metadata.get(key)]
    if missing:
        raise ReleaseError("metadata requires: " + ", ".join(missing))
    if not release.get("archive_name") or not release.get("archive_root"):
        raise ReleaseError("release archive_name and archive_root are required")
    if not document.get("allowlist") or not document.get("denylist"):
        raise ReleaseError("allowlist and denylist must be non-empty")


def matches(pattern, relative):
    return fnmatch.fnmatch(relative, pattern) or fnmatch.fnmatch(
        "/" + relative, pattern
    )


def collect_sources(package, document):
    allowlist = sorted(set(document["allowlist"]))
    denylist = document["denylist"]
    actual = sorted(
        path.relative_to(package).as_posix()
        for path in package.rglob("*")
        if path.is_file() and not any(
            matches(pattern, path.relative_to(package).as_posix())
            for pattern in denylist
        )
    )
    unexpected = sorted(set(actual) - set(allowlist))
    missing = sorted(set(allowlist) - set(actual))
    denied = [relative for relative in allowlist
              if any(matches(pattern, relative) for pattern in denylist)]
    if denied:
        raise ReleaseError("allowlist intersects denylist: " +
                           ", ".join(denied))
    if unexpected:
        raise ReleaseError("unallowlisted package files: " +
                           ", ".join(unexpected))
    if missing:
        raise ReleaseError("allowlisted files missing: " + ", ".join(missing))
    return [package / relative for relative in allowlist]


def manifest_text(package, files, archive_root):
    lines = []
    for path in files:
        relative = path.relative_to(package).as_posix()
        lines.append(sha256(path) + "  " + archive_root + "/" + relative)
    return "\n".join(lines) + "\n"


def zip_write(bundle, arcname, content):
    info = zipfile.ZipInfo(arcname, (1980, 1, 1, 0, 0, 0))
    info.compress_type = zipfile.ZIP_DEFLATED
    info.external_attr = 0o100644 << 16
    bundle.writestr(info, content)


def build_bundle(package, document, output):
    validate_metadata(document)
    files = collect_sources(package, document)
    output.mkdir(parents=True, exist_ok=True)
    archive = output / document["release"]["archive_name"]
    manifest = output / "MANIFEST-sha256.txt"
    text = manifest_text(package, files, document["release"]["archive_root"])
    manifest.write_text(text, encoding="utf-8", newline="\n")
    with zipfile.ZipFile(archive, "w", zipfile.ZIP_DEFLATED) as bundle:
        for path in files:
            name = document["release"]["archive_root"] + "/" + \
                path.relative_to(package).as_posix()
            zip_write(bundle, name, path.read_bytes())
        zip_write(bundle, "MANIFEST-sha256.txt", text.encode("utf-8"))
    return archive, manifest


def verify_bundle(archive, manifest, document):
    expected = {
        line.split("  ", 1)[1]: line.split("  ", 1)[0]
        for line in manifest.read_text(encoding="utf-8").splitlines()
    }
    expected_names = list(expected) + ["MANIFEST-sha256.txt"]
    with zipfile.ZipFile(archive) as bundle:
        if bundle.namelist() != expected_names:
            raise ReleaseError("archive filename or count mismatch")
        for name, checksum in expected.items():
            if hashlib.sha256(bundle.read(name)).hexdigest() != checksum:
                raise ReleaseError("SHA-256 mismatch for " + name)
        if bundle.read("MANIFEST-sha256.txt") != manifest.read_bytes():
            raise ReleaseError("archive manifest differs from local manifest")
    if len(expected) != len(document["allowlist"]):
        raise ReleaseError("manifest count differs from allowlist count")


def load_dotenv_value(env_file, name):
    if env_file is None or not env_file.is_file():
        return None
    for raw in env_file.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        if key.strip() == name:
            return value.strip().strip("\"").strip("'")
    return None


def load_token(environment, env_file):
    name = ("ZENODO_API_KEY" if environment == "production"
            else "ZENODO_SANDBOX_API_KEY")
    token = os.environ.get(name) or load_dotenv_value(env_file, name)
    if not token:
        raise ReleaseError(name + " is required for " + environment)
    return token


def read_state(path):
    if not path.is_file():
        raise ReleaseError("state file does not exist: " + str(path))
    return read_metadata(path)


def write_state(path, state):
    allowed = {"environment", "record_id", "draft_url", "record_url",
               "reserved_doi"}
    if set(state) - allowed:
        raise ReleaseError("state contains a forbidden field")
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(state, indent=2) + "\n", encoding="utf-8")
    os.chmod(path, 0o600)


def verify_remote_file(entry, content):
    if entry.get("status") != "completed":
        raise ReleaseError("Zenodo file status is not completed")
    if entry.get("size") != len(content):
        raise ReleaseError("Zenodo file size mismatch")
    if entry.get("checksum") != "md5:" + md5_bytes(content):
        raise ReleaseError("Zenodo MD5 checksum mismatch")


def require_publish_confirmation(confirm, approved):
    if not confirm:
        raise ReleaseError("publish requires --confirm-publish")
    if not approved:
        raise ReleaseError("publish requires --user-approved")


class ZenodoClient:
    def __init__(self, environment, token):
        self.base = API_BASES[environment]
        self.token = token

    def request(self, method, path, body=None, content_type="application/json"):
        url = path if path.startswith("https://") else self.base + path
        data = None if body is None else (
            body if isinstance(body, bytes) else json.dumps(body).encode("utf-8")
        )
        request = urllib.request.Request(
            url, data=data, method=method,
            headers={"Authorization": "Bearer " + self.token,
                     "Content-Type": content_type,
                     "Accept": "application/vnd.inveniordm.v1+json"},
        )
        try:
            with urllib.request.urlopen(request, timeout=60) as response:
                payload = response.read()
                return response.status, json.loads(payload) if payload else {}
        except urllib.error.HTTPError as error:
            detail = error.read().decode("utf-8", errors="replace")
            raise ReleaseError(
                "Zenodo " + str(error.code) + ": " + detail
            ) from error

    def create_draft(self, metadata):
        payload = {"access": {"record": "public", "files": "public"},
                   "files": {"enabled": True}, "metadata": metadata}
        status, response = self.request("POST", "/records", payload)
        if status != 201:
            raise ReleaseError("draft creation did not return 201")
        return response

    def reserve_doi(self, record_id):
        status, response = self.request(
            "POST", "/records/" + record_id + "/draft/pids/doi", {}
        )
        if status != 201 or not response.get("pids", {}).get("doi", {}).get("identifier"):
            raise ReleaseError("DOI reservation failed")
        return response["pids"]["doi"]["identifier"]

    def upload(self, record_id, files):
        keys = [{"key": path.name} for path in files]
        status, response = self.request(
            "POST", "/records/" + record_id + "/draft/files", keys
        )
        if status != 201:
            raise ReleaseError("file initialization did not return 201")
        entries = {entry["key"]: entry for entry in response["entries"]}
        if set(entries) != {path.name for path in files}:
            raise ReleaseError("Zenodo initialized an unexpected file set")
        for path in files:
            entry = entries[path.name]
            status, _ = self.request(
                "PUT", entry["links"]["content"], path.read_bytes(),
                "application/octet-stream"
            )
            if status not in (200, 201):
                raise ReleaseError("file content upload did not return 2xx")
            status, committed = self.request("POST", entry["links"]["commit"], {})
            if status != 200:
                raise ReleaseError("file commit did not return 200")
            verify_remote_file(committed, path.read_bytes())

    def list_files(self, record_id, published=False):
        suffix = "/files" if published else "/draft/files"
        status, response = self.request("GET", "/records/" + record_id + suffix)
        if status != 200:
            raise ReleaseError("file listing did not return 200")
        return response.get("entries", [])

    def publish(self, record_id):
        status, response = self.request(
            "POST", "/records/" + record_id + "/draft/actions/publish", {}
        )
        if status != 202 or not response.get("is_published"):
            raise ReleaseError("publish did not return a published record")
        return response


def metadata_payload(document):
    validate_metadata(document)
    payload = dict(document["metadata"])
    payload["publication_date"] = date.today().isoformat()
    payload["version"] = document["release"]["version"]
    return payload


def create_draft_and_reserve(client, environment, metadata, state_path):
    draft = client.create_draft(metadata)
    state = {
        "environment": environment,
        "record_id": str(draft["id"]),
        "draft_url": draft["links"]["self_html"],
    }
    write_state(state_path, state)
    state["reserved_doi"] = client.reserve_doi(state["record_id"])
    write_state(state_path, state)
    return state


def parser():
    result = argparse.ArgumentParser()
    result.add_argument("command", choices=(
        "validate", "create-draft", "upload", "verify", "publish", "state"
    ))
    result.add_argument("--environment", choices=("sandbox", "production"))
    result.add_argument("--repo", type=Path, default=Path.cwd())
    result.add_argument("--env-file", type=Path,
                        default=Path.home() / "PhD" / ".env")
    result.add_argument("--confirm-create-draft", action="store_true")
    result.add_argument("--reserve-doi", action="store_true")
    result.add_argument("--confirm-publish", action="store_true")
    result.add_argument("--user-approved", action="store_true")
    return result


def main(argv=None):
    args = parser().parse_args(argv)
    repo = args.repo.resolve()
    package = repo / "osf_storage"
    document = read_metadata(package / "zenodo_metadata.json")
    state_path = repo / document["release"]["state_file"]
    output = repo / ".zenodo-release"
    if args.command == "validate":
        validate_metadata(document)
        collect_sources(package, document)
        archive, manifest = build_bundle(package, document, output)
        verify_bundle(archive, manifest, document)
        print(str(archive))
        print(str(manifest))
        return
    if args.command == "state":
        print(json.dumps(read_state(state_path), indent=2))
        return
    if not args.environment:
        raise ReleaseError("--environment is required for network stages")
    if args.command == "publish":
        require_publish_confirmation(args.confirm_publish, args.user_approved)
    if args.command == "create-draft":
        if not args.confirm_create_draft or not args.reserve_doi:
            raise ReleaseError("draft creation requires both confirmation flags")
        if state_path.exists():
            raise ReleaseError(
                "state file already exists; resolve the existing draft first"
            )
    token = load_token(args.environment, args.env_file)
    client = ZenodoClient(args.environment, token)
    if args.command == "create-draft":
        state = create_draft_and_reserve(
            client, args.environment, metadata_payload(document), state_path
        )
        print(state["reserved_doi"])
        return
    state = read_state(state_path)
    if state["environment"] != args.environment:
        raise ReleaseError("state environment does not match command environment")
    archive, manifest = build_bundle(package, document, output)
    if args.command == "upload":
        client.upload(state["record_id"], [archive, manifest])
        return
    published = args.command == "verify" and "record_url" in state
    remote_files = client.list_files(state["record_id"], published)
    expected = {archive.name: archive, manifest.name: manifest}
    if {entry["key"] for entry in remote_files} != set(expected):
        raise ReleaseError("remote filename or count mismatch")
    for entry in remote_files:
        verify_remote_file(entry, expected[entry["key"]].read_bytes())
    if args.command == "publish":
        published = client.publish(state["record_id"])
        state["record_url"] = published["links"]["self_html"]
        write_state(state_path, state)


if __name__ == "__main__":
    try:
        main()
    except ReleaseError as error:
        print("release validation failed: " + str(error), file=sys.stderr)
        raise SystemExit(2)
