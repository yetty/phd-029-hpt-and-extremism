# OSF to Zenodo Migration Implementation Plan
Status: Draft

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Publish a validated, privacy-safe Zenodo reproducibility package
from `osf_storage/`, while retaining the OSF preregistration as the
authoritative record of the prospective study plan.

**Architecture:** Treat the current `osf_storage/` tree as the only release
source. A small standard-library Python tool will create an explicit archive,
manifest, and non-secret local state, then operate only on Zenodo's current
InvenioRDM Records API. Its stages are deliberately separate: local
validation, draft/DOI reservation, upload, verification, and publication.
No stage alters OSF; every OSF-side action is isolated behind a later,
separate approval gate.

**Tech Stack:** Python 3 standard library, JSON, `unittest`, GNU Make, R,
XeLaTeX, `sha256sum`, `md5sum`, Git, Zenodo InvenioRDM Records API.

---

## File map and operating boundaries

- Create: `tools/zenodo_release.py` - deterministic bundle construction,
  manifest checks, secure token loading, and Records API stages.
- Create: `tests/test_zenodo_release.py` - local unit tests with no network
  access and no credentials.
- Create: `osf_storage/zenodo_metadata.json` - reviewed metadata and the
  complete release allowlist and approved licence split.
- Create: `osf_storage/scripts/LICENSE` - MIT licence for analysis code; the
  Zenodo record-level licence is CC BY 4.0, with the README defining its scope
  and excluding third-party instrument files pending permission.
- Modify: `.gitignore` - exclude the release workspace and non-secret draft
  state from Git and from the release allowlist.
- Modify after DOI reservation: `osf_storage/README.md`,
  `submissions/pci_psychology/manuscript.tex`,
  `submissions/pci_psychology/cover_letter.md`,
  `submissions/pci_psychology/top_disclosure_table.md`,
  `submissions/pci_psychology/submission_checklist.md`,
  `submissions/pci_psychology/test_revision_reporting_analyses.R`,
  `status.md`, `project_status.md`, `docs/study-and-pipeline.md`, and
  `CLAUDE.md`.
- Generated and ignored: `.zenodo-release/` - archive, manifest, downloaded
  verification copies, and no source data outside `osf_storage/`.
- Generated and ignored: `.zenodo-release/state.json` - only the
  Zenodo record ID, draft URL, record URL, reserved DOI, and environment. It
  must never contain a token, request headers, or response bodies.

The bundle's source list is the `allowlist` in the metadata file. After the
metadata and licence files are added, it must be identical to tracked files
under `osf_storage/` except for the explicitly denied `instruments/` subtree.
It must reject any untracked, non-denied package file. The denylist excludes
confidential-name patterns, teacher-report paths, raw source data names,
third-party instrument files, the state file, release workspaces, and ignored
build intermediates; it also rejects any allowlist entry matching those
patterns.
Do not rename `osf_storage/`; its stable name is already coupled to the
analytical scripts and tests.

The four approval gates are intentionally not interchangeable:

1. **Gate A: OSF privacy containment.** Obtain explicit approval before any
   OSF visibility, add-on, file-removal, or deletion action. The
   preregistration is out of scope for all such actions.
2. **Gate B: production draft and DOI reservation.** Obtain explicit approval
   before creating a production Zenodo draft or reserving its DOI.
3. **Gate C: Zenodo publication.** Obtain explicit approval immediately before
   the publish command, after review of the exact manifest, metadata, rights,
   and Zenodo draft.
4. **Gate D: OSF landing-page, add-on, resource-link, or redirect changes.**
   Obtain a new explicit approval only after Zenodo post-publication checks
   pass. The approved plan retains the OSF Project DOI, so redirects remain
   out of scope.

Official OSF guidance checked on 2026-10-20 confirms that Project writes end
and Projects become read-only on 2027-02-19. Recheck the date against current
official OSF guidance immediately before any later OSF execution. This date
does not authorize an OSF change or shorten any approval gate.

### Task 1: Make the urgent OSF privacy-containment decision first

**Files:**
- Create outside Git, only if approved: a dated OSF export and provenance
  folder selected by the user, with access restricted to the project owner.
- Do not modify: the OSF preregistration, its DOI, or its registration record.
- Do not modify locally: repository files before the decision is recorded by
  the user outside the public repository.

- [x] **Step 1: Stop ordinary migration work and present Gate A.**

  Explain only that the distinct OSF Project and linked storage integration
  require privacy remediation because they may expose obsolete public content.
  Do not enumerate labels, filenames, people, sources, or repository entries.
  Reconfirm the approved action: remove all files from the separate OSF
  Project storage, remediate the linked add-on that exposes repository
  content, retain the Project page and DOI, and leave a neutral pointer after
  the Zenodo DOI exists. The user may approve execution now or defer it. State
  that the OSF preregistration will remain public, authoritative, unwithdrawn,
  and unchanged.

  Expected outcome: a written user decision that names the approved OSF
  Project action and explicitly excludes the preregistration.

- [x] **Step 2: Verify the OSF timing assumption before the approved action.**

  Read the official OSF Projects transition guidance on the execution day and
  record, outside Git, the page URL, access date, the confirmed Project-write
  cutoff, and the confirmed read-only date. If either date differs from the
  assumptions above, stop and obtain a new scheduling decision.

  Expected outcome: dated evidence that the chosen OSF Project action remains
  possible. No API call or browser automation is permitted until Gate A is
  explicitly approved.

- [x] **Step 3: Capture non-public provenance before a destructive OSF action.**

  Use OSF's supported export/download facilities to capture the Project file
  inventory, metadata, wikis, and activity information. Store raw exports
  outside this repository and outside Git because historical versions may
  contain obsolete or sensitive material. Store a local access-restricted
  note containing hashes, export date, and the generic action approved; do
  not put exposed file names or contents in the public release package.

  Expected outcome: recoverable evidence of the OSF Project state without
  making it part of the Zenodo payload or the Git history.

- [x] **Step 4: Execute only the approved OSF Project remediation and verify
  it independently.**

  If the approval is to change visibility, confirm the Project's new
  visibility from a signed-out session. If the approval is to remove Project
  or linked-add-on material, confirm that each approved surface no longer
  serves it. Do not overwrite in place as a substitute for removal. Do not
  alter the preregistration, request a redirect, add a Resource link, or make
  a landing-page change in this task.

  Expected outcome: Gate A is completed or explicitly deferred by the user;
  the preregistration remains intact.

- [x] **Step 5: Do not commit this external decision.**

  There is intentionally no Git commit for this gate: raw provenance belongs
  outside Git, and this task must complete before ordinary repository edits.
  Confirm `git status --short` still leaves existing unrelated `reports/` and
  `research_notes/` files untouched.

### Task 2: Record the approved licensing and metadata contract

**Files:**
- Create: `osf_storage/zenodo_metadata.json`
- Create: `osf_storage/scripts/LICENSE`
- Modify: `osf_storage/README.md`
- Modify: `.gitignore`

- [x] **Step 1: Verify the remaining instrument redistribution terms.**

  The approved decision after rights review is CC BY 4.0 for data, original
  documentation, figures, and rendered outputs, with MIT for code. Exclude all
  files under `osf_storage/instruments/` pending documented redistribution
  permission. Do not assume a grant, Zenodo community, coauthor, or funding
  information.

  Expected outcome: evidence that the approved split covers every allowlisted
  file. The user approved exclusion of third-party instrument files on
  2026-10-20 after the rights review triggered this stop condition.

- [x] **Step 2: Add the metadata file with the approved licence split.**

  Use CC BY 4.0 as Zenodo's record-level licence and state in the description
  that code under `scripts/` is MIT-licensed. Use this complete file:

  ```json
  {
    "release": {
      "archive_name": "hpt-replication-package.zip",
      "archive_root": "replication_package",
      "state_file": ".zenodo-release/state.json",
      "release_doi": "",
      "version": "1.0"
    },
    "metadata": {
      "title": "Cross-Cultural Validation and Ideological Fairness of a Historical Perspective Taking Instrument: Evidence from Czech Secondary Students",
      "description": "De-identified data, analysis code, rendered outputs, and documentation for a preregistered study of historical perspective taking. The OSF preregistration remains the authoritative record of the prospective study plan. Data, original documentation, figures, and rendered outputs are licensed CC BY 4.0; code under scripts/ is licensed under the MIT License. Third-party instrument files and item text are excluded pending documented redistribution permission.",
      "creators": [
        {
          "person_or_org": {
            "type": "personal",
            "given_name": "Juda",
            "family_name": "Kaleta",
            "identifiers": [
              {
                "scheme": "orcid",
                "identifier": "0000-0003-0043-4039"
              }
            ]
          },
          "affiliations": [
            {
              "name": "Charles University, Faculty of Arts, Institute of History"
            }
          ]
        }
      ],
      "subjects": [
        {"subject": "historical perspective taking"},
        {"subject": "cross-cultural validation"},
        {"subject": "measurement invariance"},
        {"subject": "differential item functioning"},
        {"subject": "construct-irrelevant variance"},
        {"subject": "test fairness"}
      ],
      "resource_type": {
        "id": "dataset"
      },
      "rights": [
        {
          "id": "cc-by-4.0"
        }
      ],
      "related_identifiers": [
        {
          "identifier": "10.17605/OSF.IO/ZSNGY",
          "scheme": "doi",
          "relation_type": {
            "id": "issupplementto"
          },
          "resource_type": {
            "id": "publication-other"
          }
        },
        {
          "identifier": "10.31234/osf.io/hxngm_v2",
          "scheme": "doi",
          "relation_type": {
            "id": "issupplementto"
          },
          "resource_type": {
            "id": "publication-preprint"
          }
        },
        {
          "identifier": "10.17605/OSF.IO/YNG37",
          "scheme": "doi",
          "relation_type": {
            "id": "isderivedfrom"
          },
          "resource_type": {
            "id": "dataset"
          }
        }
      ]
    },
    "allowlist": [
      "README.md",
      "instrument_adaptation_and_deviations.md",
      "supplementary_materials.md",
      "supplementary_materials.pdf",
      "zenodo_metadata.json",
      "data/codebook.pdf",
      "data/codebook_source.tex",
      "data/student_responses.RDS",
      "data/student_responses.xlsx",
      "figures/fig02_measurement_invariance_and_dif.pdf",
      "figures/fig02_measurement_invariance_and_dif.png",
      "figures/fig03_score_distributions.pdf",
      "figures/fig03_score_distributions.png",
      "figures/fig04_coefficient_plot.pdf",
      "figures/fig04_coefficient_plot.png",
      "figures/fig05_marginal_effects.pdf",
      "figures/fig05_marginal_effects.png",
      "outputs/01_measurement_checks.pdf",
      "outputs/02_descriptives_and_zero_order_correlations.pdf",
      "outputs/03_multilevel_models_hypothesis_tests.pdf",
      "outputs/04_dif_and_mg_cfa_measurement_bias.pdf",
      "outputs/05_sensitivity_analyses.pdf",
      "outputs/06_appendix_tables_and_figures.pdf",
      "outputs/07_reproducibility_report.pdf",
      "outputs/table_s1_irt_parameters.csv",
      "outputs/revision/revision_correlation_intervals.csv",
      "outputs/revision/revision_ideology_thresholds.csv",
      "outputs/revision/revision_knowledge_facilities.csv",
      "outputs/revision/revision_participant_counts.csv",
      "outputs/revision/revision_participant_descriptives.csv",
      "outputs/revision/revision_relationships.pdf",
      "outputs/revision/revision_relationships.png",
      "outputs/revision/revision_reliability.csv",
      "scripts/01_measurement_checks.Rmd",
      "scripts/02_descriptives_and_zero_order_correlations.Rmd",
      "scripts/03_multilevel_models_hypothesis_tests.Rmd",
      "scripts/04_dif_and_mg_cfa_measurement_bias.Rmd",
      "scripts/05_sensitivity_analyses.Rmd",
      "scripts/06_appendix_tables_and_figures.Rmd",
      "scripts/07_reproducibility_report.Rmd",
      "scripts/Makefile",
      "scripts/LICENSE",
      "scripts/H1_ideology_elevates_HPT.R",
      "scripts/compute_instruments_validity.R",
      "scripts/factor_and_invariance.R",
      "scripts/fig02_measurement_invariance_and_dif.R",
      "scripts/fig03_score_distributions.R",
      "scripts/fig04_coefficient_plot.R",
      "scripts/fig05_marginal_effects.R",
      "scripts/multilevel_and_DIF.R",
      "scripts/revision_reporting_analyses.R",
      "scripts/scoring_helpers.R",
      "scripts/supplementary_analyses.R",
      "scripts/tost_equivalence_tests_and_mundlak.R"
    ],
    "denylist": [
      "**/*CONFIDENTAL*",
      "**/*CONFIDENTIAL*",
      "**/teacher_report.*",
      "**/pull_and_normalize_data_*",
      "**/normalised_responses.*",
      "instruments/**",
      "**/.zenodo-release-state.json",
      "**/.zenodo-release/**",
      "**/*.aux",
      "**/*.fdb_latexmk",
      "**/*.fls",
      "**/*.log",
      "**/*.out",
      "**/.DS_Store",
      "**/__MACOSX/**",
      "**/Thumbs.db"
    ]
  }
  ```

- [x] **Step 3: Add the MIT licence for code.**

  Create `osf_storage/scripts/LICENSE` with this exact text:

  ```text
  MIT License

  Copyright (c) 2026 Juda Kaleta

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
  ```

  Add a licence section to `osf_storage/README.md` stating that data, original
  documentation, figures, and rendered outputs are CC BY 4.0, code under
  `scripts/` is MIT-licensed, and third-party instrument files and item text
  are excluded pending documented redistribution permission. Do not claim
  those terms for any excluded source material.

- [x] **Step 4: Add ignored release-only paths without broad ignore rules.**

  Append exactly these lines to `.gitignore`:

  ```gitignore

  # Zenodo release workspace and non-secret remote draft state
  .zenodo-release/
  .zenodo-release/state.json
  ```

  Run:

  ```bash
  git check-ignore -v .zenodo-release/example \
    .zenodo-release/state.json
  git diff --check
  ```

  Expected: both test paths match the two new rules and `git diff --check`
  exits successfully.

- [x] **Step 5: Commit the reviewed release contract.**

  Run:

  ```bash
  git add .gitignore osf_storage/zenodo_metadata.json \
    osf_storage/scripts/LICENSE osf_storage/README.md
  git commit -m "reproducibility: define Zenodo release contract"
  git push origin main
  git -C /home/yetty/PhD add projects/phd-029-hpt-and-extremism
  git -C /home/yetty/PhD commit -m "projects: update phd-029 release contract"
  git -C /home/yetty/PhD push origin main
  ```

  Expected: one small commit containing only the ignore rules and metadata
  contract. Do not create a draft until Task 5 and Gate B.

### Task 3: Write failing local tests for bundle, secrets, and API guards

**Files:**
- Create: `tests/test_zenodo_release.py`
- Create later in Task 4: `tools/zenodo_release.py`

- [x] **Step 1: Add the complete failing test module.**

  ```python
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
          client.request.assert_called_once_with("GET", "/records/123/files")

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
  ```

- [x] **Step 2: Run the focused test to prove the missing implementation.**

  Run:

  ```bash
  /home/yetty/PhD/.venv/bin/python tests/test_zenodo_release.py -v
  ```

  Expected: FAIL because `tools/zenodo_release.py` does not yet exist. The
  test must make no network request and must not load `/home/yetty/PhD/.env`.

### Task 4: Implement the local release tool and make the tests pass

**Files:**
- Create: `tools/zenodo_release.py`
- Modify: `tests/test_zenodo_release.py` only if the failing test exposed a
  real error in the test fixture rather than a missing implementation.

- [x] **Step 1: Implement source validation, deterministic packaging, and
  local verification.**

  Write `tools/zenodo_release.py` with this complete implementation. It uses
  only the standard library, confines every source to `osf_storage/`, and
  never opens an environment file until a network stage needs a token.

  ```python
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
      name = "ZENODO_API_KEY"
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
      result.add_argument("--environment", choices=("production",))
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
  ```

- [x] **Step 2: Run the test and local validation.**

  Run:

  ```bash
  /home/yetty/PhD/.venv/bin/python tests/test_zenodo_release.py -v
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py validate --repo .
  ```

  Expected: unit tests and local validation pass. Validation builds only the
  allowlisted files, excludes ignored denylisted intermediates, and neither
  reads an environment file nor calls Zenodo.

- [x] **Step 3: Commit the tested local release tool.**

  Run:

  ```bash
  git add tools/zenodo_release.py tests/test_zenodo_release.py
  git commit -m "reproducibility: add guarded Zenodo release tool"
  git push origin main
  git -C /home/yetty/PhD add projects/phd-029-hpt-and-extremism
  git -C /home/yetty/PhD commit -m "projects: update phd-029 release tool"
  git -C /home/yetty/PhD push origin main
  ```

  Expected: one commit containing only the tool and its isolated unit tests.

### Task 5: Complete a production-only local preflight

**Files:**
- Verify: `osf_storage/zenodo_metadata.json`
- Modify: `tools/zenodo_release.py`
- Modify: `tests/test_zenodo_release.py`
- Generated and ignored: `.zenodo-release/`

- [x] **Step 1: Recheck the reviewed metadata contract.**

  Confirm that the record-level right is `cc-by-4.0`, the description states
  the MIT split for code, `scripts/LICENSE` is allowlisted, and the approved
  related identifiers are present, and third-party instrument files remain
  denied and absent from the allowlist. Add `metadata.funding` only when the
  user supplies a verifiable award identifier and funder name. Do not add
  communities, contributors, or grants without user-supplied evidence.

- [x] **Step 2: Harden the parent environment file without displaying it.**

  Run exactly:

  ```bash
  chmod 600 /home/yetty/PhD/.env
  test "$(stat -c '%a' /home/yetty/PhD/.env)" = 600
  ```

  Expected: the command produces no secret output and exits zero. Do not use
  `cat`, `grep`, `printenv`, shell tracing, or any command that emits token
  values. This hardens the existing production token file.

- [x] **Step 3: Remove the unused test-environment path.**

  The user explicitly declined a separate test account on 2026-10-20. Remove
  the test API base, credential name, parser choice, and workflow. Add a
  failing parser test first, then retain production as the only accepted
  environment. This reduces remote integration assurance but does not weaken
  Gate B or Gate C; the production draft remains unpublished until Gate C.

- [x] **Step 4: Run the full local production preflight.**

  Run:

  ```bash
  /home/yetty/PhD/.venv/bin/python tests/test_zenodo_release.py -v
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py validate --repo .
  wc -l .zenodo-release/MANIFEST-sha256.txt
  unzip -Z1 .zenodo-release/hpt-replication-package.zip | sort
  ! unzip -Z1 .zenodo-release/hpt-replication-package.zip | \
    grep -q 'replication_package/instruments/'
  ```

  Expected: all tests pass, validation creates exactly one archive and one
  manifest from the 54-file allowlist, the ZIP contains the 54 payload files
  plus `MANIFEST-sha256.txt`, and no instrument path appears. No network call
  or credential read occurs.

- [x] **Step 5: Commit the production-only preflight change.**

  Run:

  ```bash
  git add tools/zenodo_release.py tests/test_zenodo_release.py \
    docs/superpowers/plans/2026-10-20-osf-to-zenodo-migration.md
  git diff --cached --check
  git commit -m "reproducibility: use production-only Zenodo workflow"
  git push origin main
  git -C /home/yetty/PhD add projects/phd-029-hpt-and-extremism
  git -C /home/yetty/PhD commit -m "projects: update phd-029 Zenodo workflow"
  git -C /home/yetty/PhD push origin main
  ```

  Expected: a focused commit containing only the production-only workflow,
  tests, and synchronized plan. Stop at Gate B before creating a production
  draft or reserving a DOI.

### Task 6: Reserve the production DOI, update live references, and rebuild

**Files:**
- Modify: `osf_storage/zenodo_metadata.json`
- Modify: `osf_storage/README.md`
- Modify: `submissions/pci_psychology/manuscript.tex`
- Modify: `submissions/pci_psychology/cover_letter.md`
- Modify: `submissions/pci_psychology/top_disclosure_table.md`
- Modify: `submissions/pci_psychology/submission_checklist.md`
- Modify: `submissions/pci_psychology/test_revision_reporting_analyses.R`
- Modify: `status.md`, `project_status.md`, `docs/study-and-pipeline.md`, and
  `CLAUDE.md`
- Regenerate: submission PDF artifacts and the exact bundle in
  `.zenodo-release/`

- [x] **Step 1: Stop for Gate B.**

  Present the exact title, description, creators, ORCID, affiliation, custom
  subjects used as Zenodo keywords, resource type, rights decision, related
  identifiers, version, publication-date behaviour, and complete allowlist
  for approval. Confirm that `publication_date` will be the production draft
  creation and DOI-reservation date. If the user requires the later publish
  date instead, stop and add a tested draft-metadata update step. Confirm
  that the OSF preregistration DOI is a related identifier, not an externally
  supplied DOI for the Zenodo record. Confirm no funding, community, or
  coauthor is being asserted. Do not run the command below without an explicit
  user approval that covers production draft creation and DOI reservation.

- [x] **Step 2: Reserve the production DOI and persist only non-secret state.**

  Run:

  ```bash
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py create-draft \
    --repo . --environment production --confirm-create-draft --reserve-doi
  test -f .zenodo-release/state.json
  test "$(stat -c '%a' .zenodo-release/state.json)" = 600
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py state --repo .
  ```

  Expected: the final command prints only the production environment, record
  ID, draft URL, and reserved DOI. It must not print an authorization header
  or token. A failure leaves an unpublished draft that may be discarded only
  after the user understands that discarding loses the reserved DOI.

- [x] **Step 3: Propagate the reserved DOI in one controlled source change.**

  Obtain the DOI without echoing any secret and update only live sources. This
  command changes the tracked metadata's `release_doi` field:

  ```bash
  DOI="$(/home/yetty/PhD/.venv/bin/python tools/zenodo_release.py state --repo . | \
    /home/yetty/PhD/.venv/bin/python -c 'import json,sys; print(json.load(sys.stdin)["reserved_doi"])')"
  /home/yetty/PhD/.venv/bin/python - "$DOI" <<'PY'
  import json
  import sys
  from pathlib import Path

  path = Path("osf_storage/zenodo_metadata.json")
  document = json.loads(path.read_text(encoding="utf-8"))
  document["release"]["release_doi"] = sys.argv[1]
  path.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")
  PY
  ```

  Replace the current OSF Project repository reference with the reserved DOI
  in the live README, manuscript data-availability and TOP rows, cover letter,
  TOP table, live submission checklist, status files, pipeline documentation,
  and project guidance.
  Preserve the OSF preregistration reference as the prospective-plan record.
  Use the final sentence `Zenodo hosts the public reproducibility package.`
  Keep this DOI-bearing change uncommitted and unpushed until publication has
  passed Gate C and all post-publication checks. Do not change archived
  submissions, old milestone descriptions, or generated redline files by
  hand.

  In `test_revision_reporting_analyses.R`, replace the current repository-DOI
  assertions with this complete R block, placed before the README assertions:

  ```r
  zenodo_metadata_lines <- readLines(
    "osf_storage/zenodo_metadata.json", warn = FALSE
  )
  release_doi_line <- zenodo_metadata_lines[grepl(
    '"release_doi"', zenodo_metadata_lines, fixed = TRUE
  )]
  stopifnot(length(release_doi_line) == 1L)
  zenodo_doi <- sub(
    '^[[:space:]]*"release_doi"[[:space:]]*:[[:space:]]*"([^"]+)".*$',
    "\\1", release_doi_line
  )
  expect_true(
    grepl("^10\\.5281/zenodo\\.[0-9]+$", zenodo_doi),
    "The tracked release metadata must contain the reserved production Zenodo DOI."
  )
  zenodo_url <- paste0("https://doi.org/", zenodo_doi)
  ```

  Replace the README assertion at current lines 268-278 with a check for
  `zenodo_url`, retain its separate preregistration assertion, and replace the
  Project-status assertion at current lines 652-660 with a check for
  `zenodo_url`. Replace the README migration assertion at current lines
  289-294 with:

  ```r
  expect_true(
    grepl(zenodo_url, readme_text, fixed = TRUE) &&
      grepl("Zenodo hosts the public reproducibility package", readme_text,
            fixed = TRUE) &&
      !grepl("prepared for a planned archival migration", readme_text,
             fixed = TRUE),
    "The README must name the published Zenodo release and not future migration."
  )
  ```

  Expected: all repository text and hard-coded test expectations are updated
  in one uncommitted working change. Nothing is pushed while the final wording
  describes a record that has not yet been published.

- [x] **Step 4: Rebuild exactly the live artifacts that consume the DOI.**

  Run:

  ```bash
  (cd osf_storage/scripts && make all && test ! -e student_responses.RDS)
  make supplement
  cp submissions/pci_psychology/supplementary_materials.{md,pdf} osf_storage/
  Rscript --vanilla test_class_identifier_sanitization.R
  Rscript --vanilla submissions/pci_psychology/test_revision_reporting_analyses.R
  bash submissions/pci_psychology/build_preprint.sh
  bash submissions/pci_psychology/build_redline.sh
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py validate --repo .
  ```

  Expected: analysis output, supplement, preprint, and redline rebuild
  successfully; the regression suite passes; package validation emits exactly
  the archive and manifest paths. Inspect the clean PDF, supplement PDF, and
  redline PDF for the DOI, live links, no unresolved references, and no
  stale migration language. If analysis outputs change statistically, stop,
  investigate, and update every affected report before proceeding.

- [ ] **Step 5: Freeze and review the DOI-bearing working change.**

  Do not commit or push before publication. Review exactly the paths that will
  be committed after Task 7 succeeds:

  ```bash
  git diff --check
  git status --short
  git diff --name-only
  ```

  Expected: the diff contains only the reviewed DOI-bearing sources and their
  regenerated artifacts. The ignored state and release workspace are absent,
  and historical submission archives remain unchanged. If Gate C is deferred,
  preserve this reviewed local change without pushing its premature wording.

### Task 7: Upload, independently verify, and publish the production record

> 2026-10-20: the user chose to commit the DOI-bearing change now and to
> publish only after the PCI resubmission edits and final rebuild
> (`submissions/pci_psychology/resubmission_checklist.md`, B6 and I3).
> Task 6 Step 5 is superseded by that decision.

**Files:**
- Generated and ignored: `.zenodo-release/hpt-replication-package.zip`
- Generated and ignored: `.zenodo-release/MANIFEST-sha256.txt`
- Generated and ignored: `.zenodo-release/state.json`

- [ ] **Step 1: Freeze the committed release and check its exact payload.**

  Run:

  ```bash
  git status --short
  Rscript --vanilla test_class_identifier_sanitization.R
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py validate --repo .
  wc -l .zenodo-release/MANIFEST-sha256.txt
  unzip -Z1 .zenodo-release/hpt-replication-package.zip | sort
  sha256sum .zenodo-release/hpt-replication-package.zip \
    .zenodo-release/MANIFEST-sha256.txt
  ```

  Expected: `git status --short` shows exactly the reviewed DOI-bearing change
  from Task 6 plus pre-existing unrelated inputs; the manifest count equals
  the metadata allowlist count; the archive contains exactly every manifest
  path plus `MANIFEST-sha256.txt`; both generated upload files have local
  SHA-256 values; and the public data pass the opaque-identifier regression
  check. Stop if any filename, count, size, checksum, or identifier check is
  unexpected.

- [ ] **Step 2: Upload the two verified release files to the production draft.**

  Run:

  ```bash
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py upload --repo . \
    --environment production
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py verify --repo . \
    --environment production
  ```

  Expected: Zenodo has exactly the archive and its SHA-256 manifest, with the
  expected filenames, count, byte sizes, `completed` statuses, and returned
  MD5 checksums matching locally computed MD5 values. This step does not
  publish the draft.

- [ ] **Step 3: Stop for Gate C before the irreversible publish call.**

  Present the user with the reserved DOI, Zenodo draft URL, version, complete
  metadata, confirmed rights, manifest file count, archive size, SHA-256
  values, and successful Zenodo MD5 checks. Obtain a new explicit approval to
  publish this exact production draft. A prior approval for DOI reservation or
  OSF remediation does not satisfy this gate.

- [ ] **Step 4: Publish once, then verify the published record.**

  Run only after Gate C:

  ```bash
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py publish --repo . \
    --environment production --confirm-publish --user-approved
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py verify --repo . \
    --environment production
  ```

  Expected: publish returns a published record, state records its public URL,
  and post-publication verification repeats filename, count, size, completed
  status, and MD5 checks. Download the public archive and manifest from the
  published landing page into `.zenodo-release/downloaded/`, extract the
  archive there, then run:

  ```bash
  (cd .zenodo-release/downloaded && \
    unzip -q hpt-replication-package.zip && \
    sha256sum -c MANIFEST-sha256.txt)
  ```

  Expected: every manifest payload hash verifies after extraction into the
  directory named in the manifest. Confirm the version DOI and concept DOI
  resolve, and inspect the landing page for the author, ORCID, affiliation,
  title, description, rights, version, date, resource type, and OSF
  preregistration related identifier.

- [ ] **Step 5: Apply the recovery policy if any check fails.**

  Before publishing, correct the local source, rebuild, validate, and either
  repair or discard the draft with explicit user acknowledgement that a
  discarded draft loses its reserved DOI. After publishing, never claim an
  unpublish operation exists. Restrict access when allowed, correct editable
  metadata, request justified remediation for privacy or copyright exposure,
  or create and publish a corrected new version that supersedes the affected
  release. The OSF preregistration remains intact in every recovery path.

- [ ] **Step 6: Commit the verified DOI-bearing release and status.**

  Update the current-work status with the Zenodo version DOI, version,
  publication date, and successful audit. Do not rewrite historical milestone
  entries that accurately describe the prior OSF state. Rerun both regression
  scripts and release validation, then stage the exact reviewed sources and
  generated artifacts:

  ```bash
  Rscript --vanilla test_class_identifier_sanitization.R
  Rscript --vanilla submissions/pci_psychology/test_revision_reporting_analyses.R
  bash submissions/pci_psychology/build_preprint.sh
  bash submissions/pci_psychology/build_redline.sh
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py validate --repo .
  git add CLAUDE.md status.md project_status.md docs/study-and-pipeline.md \
    osf_storage/README.md osf_storage/zenodo_metadata.json \
    osf_storage/supplementary_materials.md \
    osf_storage/supplementary_materials.pdf \
    submissions/pci_psychology/manuscript.tex \
    submissions/pci_psychology/manuscript.pdf \
    submissions/pci_psychology/preprint.pdf \
    submissions/pci_psychology/manuscript_diff.tex \
    submissions/pci_psychology/manuscript_diff.pdf \
    submissions/pci_psychology/cover_letter.md \
    submissions/pci_psychology/top_disclosure_table.md \
    submissions/pci_psychology/submission_checklist.md \
    submissions/pci_psychology/test_revision_reporting_analyses.R
  git diff --name-only -z -- osf_storage/outputs | xargs -0 -r git add --
  git diff --cached --check
  git commit -m "reproducibility: publish Zenodo release"
  git push origin main
  git -C /home/yetty/PhD add projects/phd-029-hpt-and-extremism
  git -C /home/yetty/PhD commit -m "projects: record phd-029 Zenodo status"
  git -C /home/yetty/PhD push origin main
  ```

  Expected: live sources and regenerated PDFs use published wording, both
  regression scripts and bundle validation pass, and no state, archive,
  downloaded file, or credential is staged.

### Task 8: Make reciprocal OSF links only with Gate D and close the audit

**Files:**
- Modify only if Gate D explicitly approves the action: live OSF registration
  Resources field and/or OSF Project landing-page/add-on settings.
- Modify if Gate D changes a live local reference: `osf_storage/README.md`,
  `status.md`, `project_status.md`, and
  `submissions/pci_psychology/test_revision_reporting_analyses.R`.
- Do not modify: archived submission directories unless a live build imports
  a specific archived file.

- [ ] **Step 1: Stop for Gate D.**

  After Task 7 has passed, present distinct choices: add only the Zenodo DOI
  to the OSF preregistration Resources field; add a neutral Project
  landing-page note; confirm the approved linked-add-on remediation; or make
  no further OSF change. Explain that redirecting either DOI is out of scope
  and that the preregistration must not be withdrawn, redirected, or altered
  as part of moving the reproducibility package.

- [ ] **Step 2: Execute only the separately approved OSF action.**

  For an approved registration Resource link, label the Zenodo DOI as data,
  code, and reproducibility materials, then verify it in a signed-out
  browser. For an approved Project/add-on remediation, repeat the independent
  privacy verification from Task 1. Do not combine Resource-link work with a
  Project deletion, visibility change, add-on change, or redirect unless the
  user separately named each action in Gate D.

- [ ] **Step 3: Run the post-publication audit and update only live docs.**

  Verify: the Zenodo version and concept DOI resolve; the Zenodo record links
  to the OSF preregistration DOI; any approved OSF Resource link resolves to
  the Zenodo version DOI; the manifest still validates; all live manuscript,
  README, cover-letter, TOP, status, and pipeline links use Zenodo for the
  package and OSF for the preregistration; and no live text presents the
  preregistered study as a journal protocol submission. Search only live
  paths, leaving archives intact:

  ```bash
  rg -ni "osf\.io/yng37|planned archival migration to Zenodo" \
    CLAUDE.md status.md project_status.md docs/study-and-pipeline.md \
    osf_storage/README.md submissions/pci_psychology/manuscript.tex \
    submissions/pci_psychology/cover_letter.md \
    submissions/pci_psychology/top_disclosure_table.md \
    submissions/pci_psychology/submission_checklist.md \
    submissions/pci_psychology/test_revision_reporting_analyses.R
  Rscript --vanilla test_class_identifier_sanitization.R
  Rscript --vanilla submissions/pci_psychology/test_revision_reporting_analyses.R
  /home/yetty/PhD/.venv/bin/python tools/zenodo_release.py validate --repo .
  ```

  Expected: search results are limited to the approved provenance relation;
  both regression scripts and release validation pass. The current approved
  resubmission migration checklist is excluded because it intentionally
  documents the old source and completed migration actions. If live
  documentation changes,
  stage exact changed paths, commit one concise audit commit, and push:

  ```bash
  git add osf_storage/README.md status.md project_status.md \
    submissions/pci_psychology/submission_checklist.md \
    submissions/pci_psychology/test_revision_reporting_analyses.R
  git commit -m "docs: audit Zenodo and OSF release links"
  git push origin main
  git -C /home/yetty/PhD add projects/phd-029-hpt-and-extremism
  git -C /home/yetty/PhD commit -m "projects: audit phd-029 Zenodo links"
  git -C /home/yetty/PhD push origin main
  ```

  Omit unchanged paths from `git add`. Do not commit unrelated planning
  research, raw OSF exports, ignored release files, or any OSF content.

## Self-review

- The first implementation task is the urgent, explicit OSF privacy decision.
- Gates A through D separately cover privacy remediation, production DOI
  reservation, Zenodo publication, and OSF link/add-on changes.
- The preregistration remains authoritative and unchanged; Zenodo contains the
  reproducibility/publication package under a new DOI.
- The tool uses only the production `/api/records` workflow. The user declined
  a separate test account, so local tests and the unpublished production draft
  checks provide the pre-publication verification.
- The `.env` hardening command verifies mode `600` without printing contents.
- `osf_storage/` is the sole source and a complete allowlist plus denylist
  rejects confidential files, teacher reports, root data, and build artifacts.
- DOI reservation precedes DOI-bearing source edits, test updates, rebuilds,
  and bundle validation.
- SHA-256 manifests, local archive checks, Zenodo MD5 checks, file count/size
  checks, Makefile rendering, and existing R regression checks all precede
  publication.
- Metadata includes the verified creator, ORCID, affiliation, title, custom
  subjects used as Zenodo keywords, description, version/date behaviour,
  resource type, and preregistration relation without inventing funding,
  coauthors, or communities.
- OSF raw export/provenance stays outside Git and the public bundle.
- Published-record recovery is correctly limited to restriction, correction,
  justified remediation, and superseding versions; no OSF preregistration
  change is used as rollback.
- Every repository task has an exact-path, small commit and push step, except
  the deliberately external Gate A decision, which creates no Git change.
- This document contains no direct identifiers, confidential data, token
  values, or exposed OSF labels; its prose is hard-wrapped at 80 columns.
