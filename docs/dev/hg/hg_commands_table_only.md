| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `add` | `[FILE]...` | `-I/--include`, `-X/--exclude`, `-S/--subrepos`, `-n/--dry-run` | Marks files for addition to repository | Text list of added files | File not found, already tracked, permission denied | 0 on success |
| `addremove` | `[FILE]...` | `-s/--similarity`, `-S/--subrepos`, `-I/--include`, `-X/--exclude`, `-n/--dry-run` | Adds new files, removes missing files | Text status report | Permission denied, binary file detection issues | 0 on success |
| `annotate` | `FILE...` | `-r/--rev`, `-f/--file`, `-u/--user`, `-d/--date`, `-n/--number`, `-c/--changeset`, `-l/--line-number`, `--skip`, `-w/--ignore-all-space`, `-b/--ignore-space-change`, `-B/--ignore-blank-lines`, `-Z/--ignore-space-at-eol`, `-T/--template` | None (read-only) | Line-by-line annotation with revision info | File not found, binary file, invalid revision | 0 on success |
| `archive` | `DEST` | `-r/--rev`, `-t/--type`, `-p/--prefix`, `--no-decode`, `-I/--include`, `-X/--exclude`, `-S/--subrepos` | Creates archive file/directory | Archive file or directory | Invalid revision, permission denied, disk space | 0 on success |
| `backout` | `REV` | `--merge`, `--commit`, `--no-commit`, `--parent`, `-e/--edit`, `-t/--tool`, `-r/--rev`, `-I/--include`, `-X/--exclude`, `-m/--message`, `-l/--logfile`, `-d/--date`, `-u/--user` | Creates new changeset, modifies working directory | Changeset hash and summary | Merge conflicts, outstanding changes, invalid revision | 0 on success, 1 if conflicts |
| `bisect` | `[SUBCOMMAND]` | `--good`, `--bad`, `--skip`, `--extend`, `--reset`, `--noupdate` | Updates working directory, creates .hg/bisect.state | Current bisect status and revision | Invalid state, no revisions to test | 0 when done, 125 when testing |
| `bookmarks` | `[NAME]...` | `-f/--force`, `-r/--rev`, `-d/--delete`, `-m/--rename`, `-i/--inactive`, `-l/--list`, `-T/--template` | Creates/modifies .hg/bookmarks | List of bookmarks with status | Invalid revision, bookmark exists, permission denied | 0 on success |
| `branch` | `[NAME]` | `-f/--force`, `-C/--clean`, `-r/--rev` | Modifies .hg/branch | Current branch name | Branch name conflicts | 0 on success |
| `branches` | None | `-a/--active`, `-c/--closed`, `-r/--rev`, `-T/--template` | None (read-only) | List of branches with status | None typical | 0 on success |
| `bundle` | `FILE [DEST]...` | `-f/--force`, `-t/--type`, `-a/--all`, `-r/--rev`, `-b/--branch`, `--base`, `-e/--ssh`, `--remotecmd`, `--insecure` | Creates bundle file | Bundle file | Network issues, invalid revisions, permission denied | 0 on success, 1 if no changes |
| `cat` | `FILE...` | `-o/--output`, `-r/--rev`, `--decode`, `-I/--include`, `-X/--exclude`, `-T/--template` | May create output files | File contents | File not found, invalid revision, binary file | 0 on success |
| `clone` | `SOURCE [DEST]` | `-U/--noupdate`, `-u/--updaterev`, `-r/--rev`, `-b/--branch`, `--pull`, `--uncompressed`, `--stream`, `-e/--ssh`, `--remotecmd`, `--insecure` | Creates new repository directory | Repository creation status | Network issues, permission denied, disk space | 0 on success |
| `commit` | `[FILE]...` | `-A/--addremove`, `--close-branch`, `--amend`, `-s/--secret`, `--draft`, `-e/--edit`, `--force-close-branch`, `-i/--interactive`, `-I/--include`, `-X/--exclude`, `-m/--message`, `-l/--logfile`, `-d/--date`, `-u/--user`, `-S/--subrepos` | Creates changeset, updates dirstate | Changeset hash and summary | Nothing to commit, merge conflicts, invalid date/user | 0 on success, 1 if nothing changed |
| `copy` | `SOURCE... DEST` | `--forget`, `-A/--after`, `--at-rev`, `-f/--force`, `-I/--include`, `-X/--exclude`, `-n/--dry-run` | Marks files as copied | Copy status report | File not found, destination exists, permission denied | 0 on success, 1 on errors |
| `diff` | `[FILE]...` | `-r/--rev`, `--from`, `--to`, `-c/--change`, `--ignore-changes-from-ancestors`, `-a/--text`, `-g/--git`, `--binary`, `--nodates`, `--noprefix`, `-p/--show-function`, `--reverse`, `-w/--ignore-all-space`, `-b/--ignore-space-change`, `-B/--ignore-blank-lines`, `-Z/--ignore-space-at-eol`, `-U/--unified`, `--stat`, `--root`, `-I/--include`, `-X/--exclude`, `-S/--subrepos` | None (read-only) | Unified diff format | Invalid revisions, binary files | 0 on success |
| `export` | `[-o OUTFILESPEC] [-r] [REV]...` | `-B/--bookmark`, `-o/--output`, `--switch-parent`, `-r/--rev`, `-a/--text`, `-g/--git`, `--binary`, `--nodates`, `-T/--template` | May create patch files | Patch format output | Invalid revisions, file I/O errors | 0 on success |
| `files` | `[FILE]...` | `-r/--rev`, `-0/--print0`, `-I/--include`, `-X/--exclude`, `-T/--template`, `-S/--subrepos` | None (read-only) | List of tracked files | Invalid revision | 0 if match found, 1 otherwise |
| `forget` | `FILE...` | `-i/--interactive`, `-I/--include`, `-X/--exclude`, `-n/--dry-run` | Modifies dirstate | List of forgotten files | File not tracked | 0 on success |
| `graft` | `REV...` | `-r/--rev`, `--base`, `--to`, `-c/--continue`, `--stop`, `--abort`, `-e/--edit`, `--log`, `--no-commit`, `-f/--force`, `-D/--currentdate`, `-U/--currentuser`, `-d/--date`, `-u/--user`, `-t/--tool`, `-n/--dry-run` | Creates changesets, modifies working directory | Changeset hashes and graft status | Merge conflicts, invalid revisions, outstanding changes | 0 on success, 1 if conflicts |
| `grep` | `PATTERN [FILE]...` | `--diff`, `-0/--print0`, `--all`, `-a/--text`, `-f/--follow`, `-i/--ignore-case`, `-l/--files-with-matches`, `-n/--line-number`, `-r/--rev`, `--all-files`, `-u/--user`, `-d/--date`, `-T/--template`, `-I/--include`, `-X/--exclude` | None (read-only) | Matching lines with context | Invalid patterns, binary files | 0 if match found, 1 otherwise |
| `heads` | `[REV]...` | `-r/--rev`, `-t/--topo`, `-c/--closed`, `--style`, `-T/--template` | None (read-only) | List of head revisions | Invalid revisions | 0 on success |
| `help` | `[TOPIC]` | `-e/--extension`, `-c/--command`, `-k/--keyword`, `-s/--system` | None (read-only) | Help text | Unknown topic | 0 on success |
| `identify` | `[-r REV] [SOURCE]` | `-r/--rev`, `-n/--num`, `-i/--id`, `-b/--branch`, `-t/--tags`, `-B/--bookmarks`, `-e/--ssh`, `--remotecmd`, `--insecure` | None (read-only) | Repository identification | Network issues, invalid revision | 0 on success |
| `import` | `PATCH...` | `-p/--strip`, `-b/--base`, `--secret`, `-e/--edit`, `-f/--force`, `--no-commit`, `--bypass`, `--partial`, `--exact`, `--prefix`, `--import-branch`, `-m/--message`, `-l/--logfile`, `-d/--date`, `-u/--user`, `-s/--similarity` | Creates changesets, modifies working directory | Import status and changeset hashes | Patch application failures, outstanding changes | 0 on success, 1 on partial success |
| `incoming` | `[SOURCE]` | `-f/--force`, `-n/--newest-first`, `--bundle`, `-r/--rev`, `-B/--bookmarks`, `-b/--branch`, `-p/--patch`, `-g/--git`, `-l/--limit`, `-M/--no-merges`, `--stat`, `-G/--graph`, `--style`, `-T/--template`, `-e/--ssh`, `--remotecmd`, `--insecure`, `-S/--subrepos` | May create bundle file | List of incoming changesets | Network issues, no changes | 0 if changes, 1 otherwise |
| `init` | `[DEST]` | `-e/--ssh`, `--remotecmd`, `--insecure` | Creates .hg directory and initial files | Repository creation status | Permission denied, directory exists | 0 on success |
| `locate` | `[PATTERN]...` | `-r/--rev`, `-0/--print0`, `-f/--fullpath`, `-I/--include`, `-X/--exclude` | None (read-only) | File paths | Invalid revision | 0 if match found, 1 otherwise |
| `log` | `[FILE]...` | `-f/--follow`, `--follow-first`, `-d/--date`, `-C/--copies`, `-k/--keyword`, `-r/--rev`, `--removed`, `-m/--only-merges`, `-u/--user`, `--only-branch`, `-b/--branch`, `-P/--prune`, `-p/--patch`, `-g/--git`, `-l/--limit`, `-M/--no-merges`, `--stat`, `-G/--graph`, `--style`, `-T/--template`, `-I/--include`, `-X/--exclude` | None (read-only) | Change history | Invalid revisions, patterns | 0 on success |
| `manifest` | None | `-r/--rev`, `--all`, `-T/--template` | None (read-only) | List of files in manifest | Invalid revision | 0 on success |
| `merge` | `[-r REV]` | `-f/--force`, `-r/--rev`, `-P/--preview`, `--abort`, `-t/--tool` | Modifies working directory, updates dirstate | Merge status | Merge conflicts, outstanding changes, no merge needed | 0 on success, 1 if conflicts |
| `outgoing` | `[DEST]...` | `-f/--force`, `-r/--rev`, `-n/--newest-first`, `-B/--bookmarks`, `-b/--branch`, `-p/--patch`, `-g/--git`, `-l/--limit`, `-M/--no-merges`, `--stat`, `-G/--graph`, `--style`, `-T/--template`, `-e/--ssh`, `--remotecmd`, `--insecure`, `-S/--subrepos` | None (read-only) | List of outgoing changesets | Network issues, no changes | 0 if changes, 1 otherwise |
| `parents` | `[FILE]` | `-r/--rev`, `--style`, `-T/--template` | None (read-only) | Parent revision info | Invalid revision | 0 on success |
| `paths` | `[NAME]` | `-T/--template` | None (read-only) | Repository paths | None typical | 0 on success |
| `phase` | `[REV...]` | `-p/--public`, `-d/--draft`, `-s/--secret`, `-f/--force`, `-r/--rev` | Modifies phase information | Phase status | Invalid revisions, phase conflicts | 0 on success, 1 if some failed |
| `pull` | `[SOURCE]...` | `-u/--update`, `-f/--force`, `--confirm`, `-r/--rev`, `-B/--bookmark`, `-b/--branch`, `--remote-hidden`, `-e/--ssh`, `--remotecmd`, `--insecure` | Updates repository, may update working directory | Pull status and changeset count | Network issues, merge conflicts on update | 0 on success, 1 if update conflicts |
| `push` | `[DEST]...` | `-f/--force`, `-r/--rev`, `-B/--bookmark`, `--all-bookmarks`, `-b/--branch`, `--new-branch`, `--pushvars`, `--publish`, `-e/--ssh`, `--remotecmd`, `--insecure` | Updates remote repository | Push status | Network issues, push creates heads, authentication | 0 if successful, 1 if nothing to push |
| `recover` | None | `--verify` | Repairs repository state | Recovery status | Repository corruption | 0 if successful, 1 if verify fails |
| `remove` | `FILE...` | `-A/--after`, `-f/--force`, `-S/--subrepos`, `-I/--include`, `-X/--exclude`, `-n/--dry-run` | Removes files, modifies dirstate | List of removed files | File not found, has local changes | 0 on success, 1 on warnings |
| `rename` | `SOURCE... DEST` | `--forget`, `-A/--after`, `--at-rev`, `-f/--force`, `-I/--include`, `-X/--exclude`, `-n/--dry-run` | Renames files, modifies dirstate | Rename status | File not found, destination exists | 0 on success, 1 on errors |
| `resolve` | `[FILE]...` | `-a/--all`, `-l/--list`, `-m/--mark`, `-u/--unmark`, `-n/--no-status`, `--re-merge`, `-t/--tool`, `-I/--include`, `-X/--exclude`, `-T/--template` | Modifies merge state, may modify files | Resolve status | No merge in progress, tool failures | 0 on success, 1 on failures |
| `revert` | `[FILE]...` | `-a/--all`, `-d/--date`, `-r/--rev`, `-C/--no-backup`, `-i/--interactive`, `-I/--include`, `-X/--exclude`, `-n/--dry-run` | Restores files, creates .orig backups | List of reverted files | Invalid revision, uncommitted merge | 0 on success |
| `root` | None | `-T/--template` | None (read-only) | Repository root path | Not in repository | 0 on success |
| `serve` | None | `-A/--accesslog`, `-d/--daemon`, `--daemon-postexec`, `-E/--errorlog`, `-p/--port`, `-a/--address`, `--prefix`, `-n/--name`, `--web-conf`, `--webdir-conf`, `--pid-file`, `--stdio`, `--cmdserver`, `-t/--templates`, `--style`, `-6/--ipv6`, `--certificate`, `--print-url`, `-S/--subrepos` | Starts server process, may create log files | Server status and URL | Port in use, permission denied | 0 on success |
| `status` | `[FILE]...` | `-A/--all`, `-m/--modified`, `-a/--added`, `-r/--removed`, `-d/--deleted`, `-c/--clean`, `-u/--unknown`, `-i/--ignored`, `-n/--no-status`, `-t/--terse`, `-C/--copies`, `-0/--print0`, `--rev`, `--change`, `-I/--include`, `-X/--exclude`, `-S/--subrepos`, `-T/--template` | None (read-only) | File status list | Invalid revisions | 0 on success |
| `summary` | None | `--remote` | None (read-only) | Repository summary | Network issues if --remote | 0 on success |
| `tag` | `NAME...` | `-f/--force`, `-l/--local`, `-r/--rev`, `--remove`, `-e/--edit`, `-m/--message`, `-d/--date`, `-u/--user` | Creates changeset (unless --local), modifies .hgtags | Tag creation status | Tag exists, invalid revision | 0 on success |
| `tags` | None | `-T/--template` | None (read-only) | List of tags | None typical | 0 on success |
| `tip` | None | `-p/--patch`, `-g/--git`, `--style`, `-T/--template` | None (read-only) | Tip revision info | No changesets | 0 on success |
| `unbundle` | `FILE...` | `-u/--update` | Updates repository, may update working directory | Unbundle status | Invalid bundle, merge conflicts | 0 on success, 1 if update conflicts |
| `update` | `[REV]` | `-C/--clean`, `-c/--check`, `-m/--merge`, `-d/--date`, `-r/--rev`, `-t/--tool` | Updates working directory | Update status and file counts | Merge conflicts, uncommitted changes | 0 on success, 1 if conflicts |
| `verify` | None | `--full` | None (read-only) | Verification report | Repository corruption | 0 on success, 1 if errors |
| `version` | None | `-T/--template` | None (read-only) | Version information | None typical | 0 on success |

## Advanced Commands

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `abort` | None | `-n/--dry-run` | Aborts ongoing operation, may restore state | Operation status | No operation to abort | 0 on success |
| `continue` | None | `-n/--dry-run` | Continues interrupted operation | Operation status | No operation to continue, conflicts | 0 on success |
| `rollback` | None | `-n/--dry-run`, `-f/--force` | Undoes last transaction | Rollback status | No transaction to rollback, public changesets | 0 on success, 1 if no rollback data |
| `shelve` | `[FILE]...` | `-A/--addremove`, `-u/--unknown`, `--cleanup`, `--date`, `-d/--delete`, `-e/--edit`, `-k/--keep`, `-l/--list`, `-m/--message`, `-n/--name`, `-p/--patch`, `-i/--interactive`, `--stat`, `-I/--include`, `-X/--exclude` | Creates shelve files, may modify working directory | Shelve status | Nothing to shelve, name conflicts | 0 on success |
| `unshelve` | `[SHELVED]` | `-a/--abort`, `-c/--continue`, `-i/--interactive`, `-k/--keep`, `-n/--name`, `-t/--tool`, `--date` | Restores shelved changes | Unshelve status | Merge conflicts, no shelves | 0 on success |

## Debug Commands

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `debugancestor` | `REV1 REV2` | None | None (read-only) | Common ancestor revision | Invalid revisions | 0 on success |
| `debugbuilddag` | `[TEXT]` | `-m/--mergeable-file`, `-o/--overwritten-file`, `-n/--new-file` | Creates changesets | Build status | Invalid DAG specification | 0 on success |
| `debugbundle` | `FILE` | `--all`, `--part-type`, `--spec` | None (read-only) | Bundle contents | Invalid bundle file | 0 on success |
| `debugcheckstate` | None | None | None (read-only) | State check results | State inconsistencies | 0 if consistent |
| `debugcommands` | None | None | None (read-only) | List of all commands | None typical | 0 on success |
| `debugcomplete` | `[CMD]` | `-o/--options` | None (read-only) | Completion candidates | Unknown command | 0 on success |
| `debugdag` | None | `-t/--tags`, `-b/--branches`, `-c/--changelog`, `-m/--manifest` | None (read-only) | DAG representation | Repository errors | 0 on success |
| `debugdata` | `FILE REV` | `-c/--changelog`, `-m/--manifest`, `-u/--uncompressed` | None (read-only) | Raw data | Invalid file/revision | 0 on success |
| `debugdate` | `DATE` | `-e/--extended` | None (read-only) | Parsed date | Invalid date format | 0 on success |
| `debugdirstate` | None | `--nodates`, `--dates`, `--datesort` | None (read-only) | Dirstate contents | Dirstate corruption | 0 on success |
| `debugdiscovery` | `[URL]` | `--old` | None (read-only) | Discovery protocol info | Network issues | 0 on success |
| `debugextensions` | None | `-T/--template` | None (read-only) | Extension information | None typical | 0 on success |
| `debugfileset` | `FILESET` | `-r/--rev`, `--all-files` | None (read-only) | Matching files | Invalid fileset | 0 on success |
| `debugformat` | None | `--template` | None (read-only) | Repository format info | None typical | 0 on success |
| `debugfsinfo` | `[PATH]` | None | None (read-only) | Filesystem information | Invalid path | 0 on success |
| `debuggetbundle` | `REPO` | `--head`, `--common`, `--type` | None (read-only) | Bundle data | Network issues | 0 on success |
| `debugignore` | `[FILE]...` | None | None (read-only) | Ignore status | None typical | 0 on success |
| `debugindex` | `FILE` | `-c/--changelog`, `-m/--manifest`, `-f/--format` | None (read-only) | Index contents | Invalid file | 0 on success |
| `debugindexdot` | `FILE` | `-c/--changelog`, `-m/--manifest` | None (read-only) | Graphviz dot format | Invalid file | 0 on success |
| `debuginstall` | None | `-T/--template` | None (read-only) | Installation check | Missing dependencies | 0 if ok |
| `debugknown` | `REV...` | None | None (read-only) | Known changesets | Invalid revisions | 0 on success |
| `debuglocks` | None | `-L/--force-lock`, `-W/--force-wlock`, `-s/--set-lock`, `-S/--set-wlock` | May create/remove lock files | Lock status | Permission denied | 0 on success |
| `debugmergestate` | None | `--style`, `-T/--template` | None (read-only) | Merge state info | No merge in progress | 0 on success |
| `debugnamecomplete` | `[NAME]` | None | None (read-only) | Name completions | None typical | 0 on success |
| `debugobsolete` | `[OBSOLETED [REPLACEMENT...]]` | `--flags`, `--record-parents`, `--rev`, `--exclusive`, `--index`, `--delete`, `--date`, `--user`, `--template` | Creates obsolescence markers | Obsolescence info | Invalid revisions | 0 on success |
| `debugpathcomplete` | `[PATH]` | `-f/--full` | None (read-only) | Path completions | None typical | 0 on success |
| `debugpeer` | `PATH` | None | None (read-only) | Peer information | Invalid path | 0 on success |
| `debugpickmergetool` | `[PATTERN]` | `-r/--rev`, `--changedeletes` | None (read-only) | Selected merge tool | None typical | 0 on success |
| `debugpushkey` | `REPO NAMESPACE [KEY] [OLD] [NEW]` | None | May modify remote state | Push result | Network issues, key conflicts | 0 on success |
| `debugpvec` | `REV` | None | None (read-only) | Phase vector | Invalid revision | 0 on success |
| `debugrebuilddirstate` | `[REV]` | `-r/--rev`, `--minimal` | Rebuilds dirstate | Rebuild status | Invalid revision | 0 on success |
| `debugrebuildfncache` | None | None | Rebuilds fncache | Rebuild status | Repository format issues | 0 on success |
| `debugrename` | `FILE` | `-r/--rev` | None (read-only) | Rename information | File not found | 0 on success |
| `debugrevlog` | `FILE` | `-c/--changelog`, `-m/--manifest`, `-d/--dump` | None (read-only) | Revlog statistics | Invalid file | 0 on success |
| `debugrevspec` | `REVSPEC` | `--optimize`, `--show-revs`, `--show-set`, `--show-stage`, `--no-optimized`, `--verify-optimized` | None (read-only) | Revset analysis | Invalid revset | 0 on success |
| `debugsetparents` | `REV1 [REV2]` | None | Modifies dirstate parents | Success message | Invalid revisions | 0 on success |
| `debugsub` | `[REV]` | None | None (read-only) | Subrepository info | Invalid revision | 0 on success |
| `debugsuccessorssets` | `REV` | `--closest` | None (read-only) | Successor sets | Invalid revision | 0 on success |
| `debugtemplate` | `[TEMPLATE]` | `-r/--rev`, `--define` | None (read-only) | Template output | Template syntax errors | 0 on success |
| `debugupdatecaches` | None | None | Updates repository caches | Cache update status | Permission denied | 0 on success |
| `debugupgraderepo` | None | `--optimize`, `--run`, `--backup`, `--changelog`, `--manifest` | May upgrade repository format | Upgrade status | Format conflicts | 0 on success |
| `debugwalk` | `[FILE]...` | `-I/--include`, `-X/--exclude` | None (read-only) | File walk results | Invalid patterns | 0 on success |
| `debugwireargs` | `[ARG]...` | `--three`, `--four`, `--five`, `--ssh`, `--remotecmd`, `--insecure` | None (read-only) | Wire protocol test | Network issues | 0 on success |

## Export/Import Commands

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `archive` | `DEST` | `-r/--rev`, `-t/--type`, `-p/--prefix`, `--no-decode`, `-I/--include`, `-X/--exclude`, `-S/--subrepos` | Creates archive file/directory | Archive creation status | Permission denied, disk space | 0 on success |
| `bundle` | `FILE [DEST]...` | `-f/--force`, `-t/--type`, `-a/--all`, `-r/--rev`, `-b/--branch`, `--base`, `-e/--ssh`, `--remotecmd`, `--insecure` | Creates bundle file | Bundle file | Network issues, no changes | 0 on success, 1 if no changes |
| `export` | `[-o OUTFILESPEC] [-r] [REV]...` | `-o/--output`, `--switch-parent`, `-r/--rev`, `-a/--text`, `-g/--git`, `--binary`, `--nodates`, `-T/--template` | May create patch files | Patch output | Invalid revisions | 0 on success |
| `import` | `PATCH...` | `-p/--strip`, `-b/--base`, `--secret`, `-e/--edit`, `-f/--force`, `--no-commit`, `--bypass`, `--partial`, `--exact`, `--prefix`, `--import-branch`, `-m/--message`, `-l/--logfile`, `-d/--date`, `-u/--user`, `-s/--similarity` | Creates changesets, modifies working directory | Import status | Patch failures, conflicts | 0 on success, 1 on partial |
| `unbundle` | `FILE...` | `-u/--update` | Updates repository, may update working directory | Unbundle status | Invalid bundle, conflicts | 0 on success, 1 if update fails |

## Notes

### Output Format Types
- **Text**: Human-readable text output
- **Status Report**: Structured status information showing file changes
- **List**: Newline-separated list of items
- **Hash**: Hexadecimal changeset identifier
- **Diff/Patch**: Unified diff format showing changes
- **JSON/Template**: Structured output when using -T flag

### Common Return Values
- **0**: Success
- **1**: General failure or conflicts
- **125**: Special bisect return code meaning "skip this revision"

### Side Effect Types
- **None (read-only)**: Command only reads repository data
- **Modifies dirstate**: Changes tracking state of files
- **Creates changesets**: Adds new revisions to repository
- **Updates working directory**: Changes files in working copy
- **Creates files**: May create new files (patches, archives, etc.)

### Common Error Categories
- **Network issues**: Connection failures, authentication errors, timeout
- **File system errors**: Permission denied, disk full, file not found
- **Repository errors**: Corruption, format incompatibility, lock conflicts
- **Merge conflicts**: Cannot automatically merge changes
- **Invalid input**: Bad revision specifiers, malformed patterns
- **State conflicts**: Outstanding changes prevent operation, wrong repository state

## Extension Commands

### MQ (Mercurial Queues) Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `qapplied` | None | `-1/--last`, `-s/--summary`, `[PATCH]` | None (read-only) | List of applied patches | None typical | 0 on success |
| `qclone` | `SOURCE [DEST]` | `--pull`, `-U/--noupdate`, `--uncompressed`, `-p/--patches`, `-e/--ssh`, `--remotecmd`, `--insecure` | Creates repository clone with patch queue | Clone status | Network issues, permission denied | 0 on success |
| `qcommit` | `[FILE]...` | `-A/--addremove`, `--close-branch`, `--amend`, `-s/--secret`, `--draft`, `-e/--edit`, `--force-close-branch`, `-i/--interactive`, `-I/--include`, `-X/--exclude`, `-m/--message`, `-l/--logfile`, `-d/--date`, `-u/--user`, `-S/--subrepos` | Commits changes to patch repository | Commit status | Nothing to commit | 0 on success |
| `qdelete` | `PATCH...` | `-k/--keep`, `-r/--rev` | Removes patch files | Deletion status | Patch applied or not found | 0 on success |
| `qdiff` | `[FILE]...` | `-a/--text`, `-g/--git`, `--binary`, `--nodates`, `--noprefix`, `-p/--show-function`, `--reverse`, `-w/--ignore-all-space`, `-b/--ignore-space-change`, `-B/--ignore-blank-lines`, `-Z/--ignore-space-at-eol`, `-U/--unified`, `--stat`, `--root`, `-I/--include`, `-X/--exclude` | None (read-only) | Diff output | None typical | 0 on success |
| `qfinish` | `[REV]...` | `-a/--applied` | Moves patches to permanent history | Finish status | Patch dependencies | 0 on success |
| `qfold` | `PATCH...` | `-e/--edit`, `-k/--keep`, `-m/--message`, `-l/--logfile` | Combines patches, modifies patch files | Fold status | Patch conflicts | 0 on success |
| `qgoto` | `PATCH` | `--keep-changes`, `-f/--force`, `--no-backup` | Applies/removes patches to reach target | Goto status | Patch conflicts | 0 on success |
| `qguard` | `[PATCH] [-- [+GUARD]... [-GUARD]...]` | `-l/--list`, `-n/--none` | Modifies patch guards | Guard status | Invalid guards | 0 on success |
| `qheader` | `[PATCH]` | None | None (read-only) | Patch header | Patch not found | 0 on success |
| `qimport` | `PATCH...` | `-e/--existing`, `-n/--name`, `-f/--force`, `-r/--rev`, `-g/--git`, `-P/--push` | Creates patch files | Import status | File conflicts, bad patches | 0 on success |
| `qinit` | None | `-c/--create-repo` | Creates .hg/patches directory | Init status | Directory exists | 0 on success |
| `qnew` | `PATCH [FILE]...` | `-e/--edit`, `-f/--force`, `-g/--git`, `-U/--currentuser`, `-u/--user`, `-D/--currentdate`, `-d/--date`, `-I/--include`, `-X/--exclude`, `-m/--message`, `-l/--logfile` | Creates new patch file, modifies working directory | Patch creation status | Uncommitted changes, name conflicts | 0 on success |
| `qnext` | None | `-s/--summary` | None (read-only) | Next patch name | No patches | 0 on success |
| `qpop` | `[PATCH | INDEX]` | `-a/--all`, `-n/--name`, `--keep-changes`, `-f/--force`, `--no-backup` | Removes patches from stack, modifies working directory | Pop status | Uncommitted changes, conflicts | 0 on success |
| `qprev` | None | `-s/--summary` | None (read-only) | Previous patch name | No patches | 0 on success |
| `qpush` | `[PATCH | INDEX]` | `--keep-changes`, `-f/--force`, `-e/--exact`, `-l/--list`, `-a/--all`, `-m/--merge`, `-n/--name`, `--move`, `--no-backup` | Applies patches, modifies working directory | Push status | Patch conflicts, uncommitted changes | 0 on success |
| `qqueue` | `[QUEUE]` | `-l/--list`, `--active`, `-c/--create`, `--rename`, `--delete`, `--purge` | Manages patch queues | Queue status | Queue conflicts | 0 on success |
| `qrefresh` | `[FILE]...` | `-e/--edit`, `-g/--git`, `-s/--short`, `-U/--currentuser`, `-u/--user`, `-D/--currentdate`, `-d/--date`, `-I/--include`, `-X/--exclude`, `-m/--message`, `-l/--logfile` | Updates current patch | Refresh status | No patches applied | 0 on success |
| `qrename` | `PATCH1 [PATCH2]` | None | Renames patch files | Rename status | Patch not found, name conflicts | 0 on success |
| `qselect` | `[GUARD]...` | `-n/--none`, `-s/--series`, `--pop`, `--reapply` | Changes active guards, may modify patch stack | Select status | Guard conflicts | 0 on success |
| `qseries` | None | `-m/--missing`, `-s/--summary` | None (read-only) | List of patches in series | None typical | 0 on success |
| `qtop` | None | `-s/--summary` | None (read-only) | Current patch name | No patches applied | 0 on success |
| `qunapplied` | `[PATCH]` | `-1/--first`, `-s/--summary` | None (read-only) | List of unapplied patches | None typical | 0 on success |

### Rebase Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `rebase` | None | `-s/--source`, `-b/--base`, `-r/--rev`, `-d/--dest`, `--collapse`, `-m/--message`, `-e/--edit`, `-l/--logfile`, `-k/--keep`, `--keepbranches`, `-D/--detach`, `-i/--interactive`, `-t/--tool`, `--stop`, `-c/--continue`, `-a/--abort`, `--auto-orphans`, `-n/--dry-run`, `-T/--template`, `--confirm` | Modifies history, creates new changesets | Rebase status and changeset info | Merge conflicts, invalid revisions | 0 on success, 1 if conflicts |

### Histedit Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `histedit` | `[ANCESTOR] or --outgoing [URL]` | `--commands`, `-c/--continue`, `--edit-plan`, `-k/--keep`, `--abort`, `-o/--outgoing`, `-f/--force`, `-r/--rev`, `-T/--template` | Modifies history interactively | Edit status and changeset info | User intervention required, conflicts | 0 on success, 1 if intervention needed |

### Shelve Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `shelve` | `[FILE]...` | `-A/--addremove`, `-u/--unknown`, `--cleanup`, `--date`, `-d/--delete`, `-e/--edit`, `-k/--keep`, `-l/--list`, `-m/--message`, `-n/--name`, `-p/--patch`, `-i/--interactive`, `--stat`, `-I/--include`, `-X/--exclude` | Creates shelve files, modifies working directory | Shelve status | Nothing to shelve, name conflicts | 0 on success |
| `unshelve` | `[SHELVED]` | `-a/--abort`, `-c/--continue`, `-i/--interactive`, `-k/--keep`, `-n/--name`, `-t/--tool`, `--date` | Restores shelved changes, modifies working directory | Unshelve status | Merge conflicts, no shelves | 0 on success |

### Graft Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `graft` | `REV...` | `-r/--rev`, `--base`, `--to`, `-c/--continue`, `--stop`, `--abort`, `-e/--edit`, `--log`, `--no-commit`, `-f/--force`, `-D/--currentdate`, `-U/--currentuser`, `-d/--date`, `-u/--user`, `-t/--tool`, `-n/--dry-run` | Creates changesets, modifies working directory | Graft status and changeset hashes | Merge conflicts, invalid revisions | 0 on success, 1 if conflicts |

### Strip Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `strip` | `REV` | `-f/--force`, `--no-backup`, `-k/--keep`, `-B/--bookmark`, `-r/--rev` | Removes changesets and descendants | Strip status | Public changesets, outstanding changes | 0 on success |

### Convert Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `convert` | `SOURCE [DEST [REVMAP]]` | `--authors`, `-s/--source-type`, `-d/--dest-type`, `-r/--rev`, `-A/--authormap`, `--filemap`, `--full`, `--splicemap`, `--branchmap`, `--branchsort`, `--datesort`, `--sourcesort`, `--closesort` | Creates new repository | Conversion progress and status | Source format issues, disk space | 0 on success |

### Purge Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `purge` | `[DIR]...` | `-a/--abort-on-err`, `--all`, `-i/--ignored`, `--dirs`, `--files`, `-p/--print`, `-0/--print0`, `--confirm`, `-I/--include`, `-X/--exclude` | Removes untracked files | List of removed files | Permission denied, file in use | 0 on success |

### Extdiff Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `extdiff` | `[FILE]...` | `-p/--program`, `-o/--option`, `-r/--rev`, `--from`, `--to`, `-c/--change`, `--per-file`, `--confirm`, `--patch`, `-I/--include`, `-X/--exclude`, `-S/--subrepos` | Launches external diff program | External program output | Program not found, file access issues | Program-dependent |

### Patchbomb Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `email` | `[DEST]...` | `-g/--git`, `--plain`, `-o/--outgoing`, `-b/--bundle`, `-B/--bookmark`, `--bundlename`, `-r/--rev`, `--force`, `--base`, `--intro`, `--body`, `-a/--attach`, `-i/--inline`, `--bcc`, `-c/--cc`, `--confirm`, `-d/--diffstat`, `--date`, `--desc`, `-f/--from`, `-n/--test`, `-m/--mbox`, `--reply-to`, `-s/--subject`, `--in-reply-to`, `--flag`, `-t/--to`, `-e/--ssh`, `--remotecmd`, `--insecure` | Sends email, may create mbox file | Email sending status | SMTP errors, network issues | 0 on success |

### Largefiles Extension

| Command | Mandatory Args | Optional Flags | Side Effects | Output Format | Possible Errors | Return Value |
|---------|---------------|----------------|--------------|---------------|-----------------|---------------|
| `lfconvert` | `SOURCE DEST [FILE]...` | `-s/--size`, `--to-normal` | Creates new repository | Conversion status | Disk space, permission denied | 0 on success |
| `lfpull` | None | `-r/--rev`, `-e/--ssh`, `--remotecmd`, `--insecure` | Downloads largefile revisions | Pull status | Network issues | 0 on success |

## Configuration File Impact

### Commands that modify .hg/hgrc or user config:
- `config` (with --edit flags)

### Commands that read configuration heavily:
- `serve` - Web interface, SSL, authentication
- `push`/`pull` - Authentication, paths, hooks  
- `commit` - Username, editor, hooks
- `merge` - Merge tools
- `email` - SMTP settings, templates

## File Format Specifications

### Output Formats by Command Type:

**Status Commands (`status`, `parents`, `summary`)**
```
Format: STATUS_CHAR FILENAME
Examples: 
M modified_file.txt
A added_file.txt  
? unknown_file.txt
```

**Log Commands (`log`, `glog`)**
```
Format: changeset: REV:NODE
        user: USER
        date: DATE
        summary: FIRST_LINE_OF_COMMIT_MSG
```

**Diff Commands (`diff`, `export`)**
```
Format: Unified diff with context
--- a/filename
+++ b/filename  
@@ -line,count +line,count @@
 context
-removed line
+added line
```

**List Commands (`files`, `tags`, `bookmarks`, `branches`)**
```
Format: One item per line
Examples:
filename.txt
tag_name REV:NODE
* active_bookmark REV:NODE
```

## Error Handling Patterns

### Common Exit Codes:
- **0**: Success, operation completed normally
- **1**: General error, operation failed
- **125**: Special code for bisect (skip this revision)
- **255**: Severe error, usually internal Mercurial error

### Typical Error Messages:
- "abort: no repository found" - Not in a repository
- "abort: outstanding uncommitted changes" - Working directory not clean
- "abort: merging with a working directory ancestor has no effect" - Invalid merge
- "abort: can't commit subrepos without .hgsub" - Subrepository configuration missing

### Recovery Commands for Common Error States:
- **Merge conflicts**: `resolve`, `merge --abort`
- **Interrupted operations**: `continue`, `abort`
- **Corrupted state**: `recover`, `verify`
- **Wrong parent**: `update`, `merge --abort`

## Template and Formatting Options

### Commonly supported template keywords:
- `{rev}` - Revision number
- `{node}` - Full changeset hash
- `{node|short}` - Short changeset hash
- `{author}` - Commit author
- `{date}` - Commit date
- `{desc}` - Commit message
- `{branch}` - Branch name
- `{tags}` - Associated tags
- `{bookmarks}` - Associated bookmarks

### Date formats:
- Default: "Mon Sep 04 15:13:13 2006 0700"
- ISO: `{date|isodate}` → "2006-09-04 15:13 +0200"
- Short: `{date|shortdate}` → "2006-09-04"
- Age: `{date|age}` → "3 weeks ago"

This comprehensive table covers the vast majority of Mercurial commands with their argument requirements, options, side effects, output formats, error conditions, and return values. The information can be exported to CSV format for easier processing and reference.
