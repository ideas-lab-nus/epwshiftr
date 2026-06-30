epwshiftr_cli_help <- function(topic = character()) {
    topic <- topic[nzchar(topic)]
    if (!length(topic)) {
        return(epwshiftr_cli_help_root())
    }
    key <- paste(topic, collapse = " ")
    registry <- epwshiftr_cli_help_registry()
    if (!key %in% names(registry)) {
        epwshiftr_cli_usage_abort(sprintf("Unknown help topic: %s", key))
    }
    registry[[key]]
}


epwshiftr_cli_help_root <- function() {
    c(
        "Usage: epwshiftr [--store PATH] [--json|--jsonl] [--quiet] <group> <command> [options]",
        "",
        "Command groups:",
        "  epwshiftr doctor",
        "  epwshiftr query help",
        "  epwshiftr download help",
        "  epwshiftr esgf help",
        "  epwshiftr storage help",
        "  epwshiftr shift help",
        "  epwshiftr extract help",
        "  epwshiftr morph help",
        "",
        "Use `epwshiftr help <group> <command>` for command-specific help."
    )
}


epwshiftr_cli_help_registry <- function() {
    list(
        doctor = c(
            "Usage: epwshiftr doctor [--network] [--index-node URL] [--timeout SECONDS]",
            "",
            "Check the local CLI, store, downloader, and optional ESGF network environment.",
            "The default checks are local and read-only; use --network to probe the index node."
        ),
        query = c(
            "Usage: epwshiftr query <command> [options]",
            "",
            "Commands:",
            "  epwshiftr query list",
            "  epwshiftr query search [--index-node URL] [--type Dataset|File|Aggregation] [--fields FIELDS] [--columns COLS] [--no-progress] [--dry-run] key=value|key!=value ...",
            "  epwshiftr query add [--index-node URL] [--label LABEL] [--tag TAG]... [--track] [--dry-run] key=value|key!=value ...",
            "  epwshiftr query add --query-file PATH [--label LABEL] [--tag TAG]... [--track] [--dry-run]",
            "  epwshiftr query show <query_id> [--files] [--updates] [--changes]",
            "  epwshiftr query status [query_id]",
            "  epwshiftr query files <query_id> [--status STATUS]",
            "  epwshiftr query updates [query_id] [--latest]",
            "  epwshiftr query changes [query_id] [--latest] [--update UPDATE_ID] [--type TYPE]",
            "  epwshiftr query tags [query_id]",
            "  epwshiftr query track <query_id>",
            "  epwshiftr query untrack <query_id>",
            "  epwshiftr query tag <query_id> <tag>... [--replace]",
            "  epwshiftr query untag <query_id> [tag...]",
            "  epwshiftr query remove <query_id> [--delete none|orphaned] [--execute]",
            "  epwshiftr query preview [query_id] [--detail]",
            "  epwshiftr query update [query_id]"
        ),
        "query list" = c(
            "Usage: epwshiftr query list",
            "",
            "List stored ESGF queries in the selected store."
        ),
        "query search" = c(
            "Usage: epwshiftr query search [--index-node URL] [--type Dataset|File|Aggregation] [--fields FIELDS] [--columns COLS] [--limit N] [--all] [--no-progress] [--dry-run] key=value|key!=value ...",
            "",
            "Run an ESGF search from command-line key=value constraints.",
            "--fields controls fields requested from ESGF; --columns controls human-readable table columns.",
            "--no-progress disables interactive progress bars.",
            "Use comma-separated values for multi-value query parameters.",
            "Use key!=value to negate a facet constraint.",
            "Use datetime_start=VALUE and datetime_stop=VALUE to constrain data coverage."
        ),
        "query add" = c(
            "Usage: epwshiftr query add [--index-node URL] [--label LABEL] [--tag TAG]... [--track] [--dry-run] key=value|key!=value ...",
            "       epwshiftr query add --query-file PATH [--label LABEL] [--tag TAG]... [--track] [--dry-run]",
            "",
            "Create an EsgQuery from command-line key=value constraints or import an EsgQuery JSON file.",
            "Use key!=value to negate a facet constraint.",
            "Use datetime_start=VALUE and datetime_stop=VALUE to constrain data coverage.",
            "This command saves query definitions only; it does not update or download ESGF files."
        ),
        "query show" = c(
            "Usage: epwshiftr query show <query_id> [--files] [--updates] [--changes]",
            "",
            "Show stored query metadata and optional related records."
        ),
        "query status" = c(
            "Usage: epwshiftr query status [query_id]",
            "",
            "Summarise tracked query, file, and download status."
        ),
        "query files" = c(
            "Usage: epwshiftr query files <query_id> [--status STATUS]",
            "",
            "List files linked to a stored query."
        ),
        "query updates" = c(
            "Usage: epwshiftr query updates [query_id] [--latest]",
            "",
            "List stored update batches for tracked queries."
        ),
        "query changes" = c(
            "Usage: epwshiftr query changes [query_id] [--latest] [--update UPDATE_ID] [--type TYPE]",
            "",
            "List per-file changes recorded by query updates."
        ),
        "query tags" = c(
            "Usage: epwshiftr query tags [query_id]",
            "",
            "List query tags."
        ),
        "query track" = c(
            "Usage: epwshiftr query track <query_id>",
            "",
            "Mark a stored query as tracked."
        ),
        "query untrack" = c(
            "Usage: epwshiftr query untrack <query_id>",
            "",
            "Mark a stored query as untracked."
        ),
        "query tag" = c(
            "Usage: epwshiftr query tag <query_id> <tag>... [--replace]",
            "",
            "Add tags to a stored query."
        ),
        "query untag" = c(
            "Usage: epwshiftr query untag <query_id> [tag...]",
            "",
            "Remove tags from a stored query."
        ),
        "query remove" = c(
            "Usage: epwshiftr query remove <query_id> [--delete none|orphaned] [--execute]",
            "",
            "Preview or remove a stored query."
        ),
        "query preview" = c(
            "Usage: epwshiftr query preview [query_id] [--detail]",
            "",
            "Preview query update changes without writing update records."
        ),
        "query update" = c(
            "Usage: epwshiftr query update [query_id]",
            "",
            "Refresh tracked query records and persist the update diff."
        ),

        shift = c(
            "Usage: epwshiftr shift <command> [options]",
            "",
            "Commands:",
            "  epwshiftr shift run --config PATH [--dry-run] [--download] [--overwrite] [--no-resume] [--no-progress]",
            "  epwshiftr shift show [--query QUERY_ID] [--plan PLAN_ID] [--morph MORPH_ID] [--files] [--outputs]",
            "  epwshiftr shift config example [--output PATH] [--overwrite]",
            "  epwshiftr shift config validate --config PATH",
            "  epwshiftr shift watch [--query QUERY_ID] [--plan PLAN_ID] [--morph MORPH_ID] [--follow] [--interval SECONDS] [--count N] [--events N]",
            "  epwshiftr shift status [--query QUERY_ID] [--plan PLAN_ID] [--morph MORPH_ID]",
            "  epwshiftr shift diagnostics [--query QUERY_ID] [--plan PLAN_ID] [--morph MORPH_ID]",
            "  epwshiftr shift outputs --morph MORPH_ID",
            "  epwshiftr shift data --plan PLAN_ID|--morph MORPH_ID [--case CASE_ID] [--columns COLS] [--limit N]"
        ),
        "shift run" = c(
            "Usage: epwshiftr shift run --config PATH [--dry-run] [--download] [--overwrite] [--no-resume] [--no-progress]",
            "",
            "Run a JSON-configured request -> collect -> optional download -> extract -> morph -> EPW workflow.",
            "The config is validated against inst/extdata/schema/shift-workflow-config.json."
        ),
        "shift show" = c(
            "Usage: epwshiftr shift show [--query QUERY_ID] [--plan PLAN_ID] [--morph MORPH_ID] [--files] [--outputs]",
            "",
            "Show the store-backed query -> extraction -> morph -> output graph."
        ),
        "shift config" = c(
            "Usage: epwshiftr shift config <example|validate> [options]",
            "",
            "Generate or validate JSON workflow configuration files."
        ),
        "shift config example" = c(
            "Usage: epwshiftr shift config example [--output PATH] [--overwrite]",
            "",
            "Print or write an example JSON workflow configuration."
        ),
        "shift config validate" = c(
            "Usage: epwshiftr shift config validate --config PATH",
            "",
            "Validate a JSON workflow configuration against the packaged schema."
        ),
        "shift watch" = c(
            "Usage: epwshiftr shift watch [--query QUERY_ID] [--plan PLAN_ID] [--morph MORPH_ID] [--follow] [--interval SECONDS] [--count N] [--events N]",
            "",
            "Return a workflow activity snapshot with query, download, extraction, morphing, output, diagnostic, and event state.",
            "Use --follow for a continuously refreshed view; combine with global --jsonl for machine-readable streaming."
        ),
        "shift status" = c(
            "Usage: epwshiftr shift status [--query QUERY_ID] [--plan PLAN_ID] [--morph MORPH_ID]",
            "",
            "Summarise store-backed workflow state by query, extraction plan, or morph ID."
        ),
        "shift diagnostics" = c(
            "Usage: epwshiftr shift diagnostics [--query QUERY_ID] [--plan PLAN_ID] [--morph MORPH_ID]",
            "",
            "Return workflow diagnostics reconstructed from store manifest state."
        ),
        "shift outputs" = c(
            "Usage: epwshiftr shift outputs --morph MORPH_ID",
            "",
            "List EPW outputs recorded for a morphing plan."
        ),
        "shift data" = c(
            "Usage: epwshiftr shift data --plan PLAN_ID|--morph MORPH_ID [--case CASE_ID] [--columns COLS] [--limit N]",
            "",
            "Preview extracted or morphed Parquet data from store manifest IDs."
        ),
        extract = c(
            "Usage: epwshiftr extract <command> [options]",
            "",
            "Commands:",
            "  epwshiftr extract plan --query QUERY_ID --site-id ID --lon LON --lat LAT --time START,STOP [--variable VARS] [--method METHOD] [--filter key=value]...",
            "  epwshiftr extract run --plan PLAN_ID[,PLAN_ID...] [--fallback auto|error] [--overwrite] [--no-resume]",
            "  epwshiftr extract retry [--plan PLAN_ID[,PLAN_ID...]] [--status failed] [--run] [--fallback auto|error] [--overwrite] [--no-resume]",
            "  epwshiftr extract coverage [--plan PLAN_ID]",
            "  epwshiftr extract artifacts --plan PLAN_ID"
        ),
        "extract plan" = c(
            "Usage: epwshiftr extract plan --query QUERY_ID --site-id ID --lon LON --lat LAT --time START,STOP [--variable VARS] [--method METHOD] [--filter key=value]...",
            "",
            "Create extraction plan rows for a stored query and target site."
        ),
        "extract run" = c(
            "Usage: epwshiftr extract run --plan PLAN_ID[,PLAN_ID...] [--fallback auto|error] [--overwrite] [--no-resume]",
            "",
            "Run selected extraction plans and write derived Parquet artifacts."
        ),
        "extract retry" = c(
            "Usage: epwshiftr extract retry [--plan PLAN_ID[,PLAN_ID...]] [--status failed] [--run] [--fallback auto|error] [--overwrite] [--no-resume]",
            "",
            "Preview or rerun extraction plans with retryable statuses."
        ),
        "extract coverage" = c(
            "Usage: epwshiftr extract coverage [--plan PLAN_ID]",
            "",
            "Inspect extraction coverage and completeness."
        ),
        "extract artifacts" = c(
            "Usage: epwshiftr extract artifacts --plan PLAN_ID",
            "",
            "List store artifact rows created by extraction results."
        ),
        morph = c(
            "Usage: epwshiftr morph <command> [options]",
            "",
            "Commands:",
            "  epwshiftr morph variables [--recipe belcher|recommended|minimal|extended]",
            "  epwshiftr morph backends",
            "  epwshiftr morph run --plan PLAN_ID[,PLAN_ID...] --epw PATH --period PERIOD=YEARS[,YEARS]... [--recipe belcher] [--reference historical|plan] [--reference-plan PLAN_ID[,PLAN_ID...]] [--reference-period PERIOD=YEARS[,YEARS]...] [--reference-filter KEY=VALUE] [--reference-option KEY=VALUE] [--strict true|false] [--by COLS] [--overwrite] [--no-resume]",
            "  epwshiftr morph epw --morph MORPH_ID [--dir DIR] [--separate true|false] [--overwrite] [--no-resume]",
            "  epwshiftr morph retry [--morph MORPH_ID[,MORPH_ID...]] [--status failed] [--run] [--overwrite] [--no-resume]",
            "  epwshiftr morph status [--morph MORPH_ID]",
            "  epwshiftr morph outputs [--morph MORPH_ID]"
        ),
        "morph variables" = c(
            "Usage: epwshiftr morph variables [--recipe belcher|recommended|minimal|extended]",
            "",
            "List CMIP variables required by a morphing recipe or variable set."
        ),
        "morph backends" = c(
            "Usage: epwshiftr morph backends",
            "",
            "List registered EPW morphing backends."
        ),
        "morph run" = c(
            "Usage: epwshiftr morph run --plan PLAN_ID[,PLAN_ID...] --epw PATH --period PERIOD=YEARS[,YEARS]... [--recipe belcher] [--reference historical|plan] [--reference-plan PLAN_ID[,PLAN_ID...]] [--reference-period PERIOD=YEARS[,YEARS]...] [--reference-filter KEY=VALUE] [--reference-option KEY=VALUE] [--strict true|false] [--by COLS] [--overwrite] [--no-resume]",
            "",
            "Run morphing through hourly Parquet outputs without writing EPW files.",
            "Use --reference historical to resolve matching CMIP historical data from the plan's collected File metadata."
        ),
        "morph epw" = c(
            "Usage: epwshiftr morph epw --morph MORPH_ID [--dir DIR] [--separate true|false] [--overwrite] [--no-resume]",
            "",
            "Write future EPW files for an existing morphing plan."
        ),
        "morph retry" = c(
            "Usage: epwshiftr morph retry [--morph MORPH_ID[,MORPH_ID...]] [--status failed] [--run] [--overwrite] [--no-resume]",
            "",
            "Preview or rerun existing morphing plans without replanning climate summaries or writing EPW files."
        ),
        "morph status" = c(
            "Usage: epwshiftr morph status [--morph MORPH_ID]",
            "",
            "List morphing plan status rows."
        ),
        "morph outputs" = c(
            "Usage: epwshiftr morph outputs [--morph MORPH_ID]",
            "",
            "List EPW output rows recorded for morphing plans."
        ),
        download = c(
            "Usage: epwshiftr download <command> [options]",
            "",
            "Commands:",
            "  epwshiftr download preflight <query_id> [--replica POLICY] [--strategy STRATEGY] [--no-probe]",
            "  epwshiftr download run <query_id> [--session-label LABEL] [--background] [--mode process|daemon] [--overwrite] [--no-resume] [--no-progress]",
            "  epwshiftr download status [--query QUERY_ID] [--session SESSION_ID]",
            "  epwshiftr download sessions",
            "  epwshiftr download tasks [--session SESSION_ID] [--job JOB_ID] [--status STATUS]",
            "  epwshiftr download events [--session SESSION_ID] [--job JOB_ID] [--task TASK_ID]",
            "  epwshiftr download watch [--query QUERY_ID] [--session SESSION_ID] [--job JOB_ID] [--follow] [--interval SECONDS] [--events N]",
            "  epwshiftr download jobs [--status STATUS]",
            "  epwshiftr download stop --job JOB_ID [--force]",
            "  epwshiftr download logs [--session SESSION_ID] [--job JOB_ID] [--task TASK_ID] [--tail N]",
            "  epwshiftr download daemon start|status|stop",
            "  epwshiftr download resume [--session SESSION_ID] [--task TASK_ID] [--overwrite] [--no-progress]",
            "  epwshiftr download verify [--session SESSION_ID] [--task TASK_ID]",
            "  epwshiftr download cancel [--session SESSION_ID] [--task TASK_ID]",
            "  epwshiftr download nodes [--service HTTPServer]",
            "  epwshiftr download reset-nodes [--node HOST] [--service HTTPServer] [--execute]",
            "  epwshiftr download retry [--query QUERY_ID] [--session SESSION_ID] [--status STATUS] [--run]",
            "  epwshiftr download config show",
            "  epwshiftr download config set [--workers N] [--timeout SECONDS] [--bandwidth-limit BYTES|none]"
        ),
        "download preflight" = c(
            "Usage: epwshiftr download preflight <query_id> [--replica POLICY] [--service SERVICE] [--strategy STRATEGY] [--probe|--no-probe]",
            "",
            "Build a replica-aware download plan without enqueuing tasks."
        ),
        "download run" = c(
            "Usage: epwshiftr download run <query_id> [--session-label LABEL] [--background] [--mode process|daemon] [--overwrite] [--no-resume] [--no-progress]",
            "",
            "Refresh, plan, enqueue, and run downloads for a stored query.",
            "Use --background to create a persistent downloader job instead of blocking the current R session."
        ),
        "download status" = c(
            "Usage: epwshiftr download status [--query QUERY_ID] [--session SESSION_ID]",
            "",
            "List downloader tasks linked to stored query files."
        ),
        "download sessions" = c(
            "Usage: epwshiftr download sessions",
            "",
            "List persistent download sessions."
        ),
        "download tasks" = c(
            "Usage: epwshiftr download tasks [--session SESSION_ID] [--job JOB_ID] [--status STATUS]",
            "",
            "List persistent download tasks."
        ),
        "download events" = c(
            "Usage: epwshiftr download events [--session SESSION_ID] [--job JOB_ID] [--task TASK_ID]",
            "",
            "List persistent downloader event records."
        ),
        "download watch" = c(
            "Usage: epwshiftr download watch [--query QUERY_ID] [--session SESSION_ID] [--job JOB_ID] [--follow] [--interval SECONDS] [--events N]",
            "",
            "Return a one-shot download activity snapshot with summary, tasks, nodes, and recent events.",
            "Use --follow for a continuously refreshed view; combine with global --jsonl for machine-readable streaming."
        ),
        "download jobs" = c(
            "Usage: epwshiftr download jobs [--status STATUS]",
            "",
            "List background downloader jobs."
        ),
        "download stop" = c(
            "Usage: epwshiftr download stop --job JOB_ID [--force]",
            "",
            "Request cancellation of a background downloader job."
        ),
        "download logs" = c(
            "Usage: epwshiftr download logs [--session SESSION_ID] [--job JOB_ID] [--task TASK_ID] [--tail N]",
            "",
            "Return the latest persistent downloader event records or background job log lines."
        ),
        "download daemon" = c(
            "Usage: epwshiftr download daemon <start|status|stop> [options]",
            "",
            "Manage the persistent downloader daemon."
        ),
        "download daemon start" = c(
            "Usage: epwshiftr download daemon start [--port PORT] [--heartbeat-interval SECONDS]",
            "",
            "Start a persistent downloader daemon for queued background jobs."
        ),
        "download daemon status" = c(
            "Usage: epwshiftr download daemon status",
            "",
            "Show downloader daemon status records."
        ),
        "download daemon stop" = c(
            "Usage: epwshiftr download daemon stop [--force]",
            "",
            "Request the active downloader daemon to stop."
        ),
        "download resume" = c(
            "Usage: epwshiftr download resume [--session SESSION_ID] [--task TASK_ID] [--overwrite] [--no-progress]",
            "",
            "Resume queued or interrupted persistent download tasks."
        ),
        "download verify" = c(
            "Usage: epwshiftr download verify [--session SESSION_ID] [--task TASK_ID]",
            "",
            "Verify checksums for completed persistent download tasks."
        ),
        "download cancel" = c(
            "Usage: epwshiftr download cancel [--session SESSION_ID] [--task TASK_ID]",
            "",
            "Cancel queued or in-progress persistent download tasks."
        ),
        "download nodes" = c(
            "Usage: epwshiftr download nodes [--service HTTPServer]",
            "",
            "List persistent data-node performance records."
        ),
        "download reset-nodes" = c(
            "Usage: epwshiftr download reset-nodes [--node HOST] [--service HTTPServer] [--execute]",
            "",
            "Preview or reset data-node performance records."
        ),
        "download retry" = c(
            "Usage: epwshiftr download retry [--query QUERY_ID] [--session SESSION_ID] [--status STATUS] [--run]",
            "",
            "Requeue failed or cancelled persistent tasks."
        ),
        "download config" = c(
            "Usage: epwshiftr download config <show|set> [options]",
            "",
            "Show or update persistent downloader configuration."
        ),
        "download config show" = c(
            "Usage: epwshiftr download config show",
            "",
            "Show persistent downloader configuration."
        ),
        "download config set" = c(
            "Usage: epwshiftr download config set [--workers N] [--retries N] [--timeout SECONDS] [--bandwidth-limit BYTES|none]",
            "",
            "Update persistent downloader configuration."
        ),
        esgf = c(
            "Usage: epwshiftr esgf <command> [options]",
            "",
            "Commands:",
            "  epwshiftr esgf report [--query QUERY_ID]"
        ),
        "esgf report" = c(
            "Usage: epwshiftr esgf report [--query QUERY_ID]",
            "",
            "Return a compact ESGF query and download health report."
        ),
        storage = c(
            "Usage: epwshiftr storage <command> [options]",
            "",
            "Commands:",
            "  epwshiftr storage layout show",
            "  epwshiftr storage layout set --layout flat|dataset|drs|template [--template TEMPLATE]",
            "  epwshiftr storage report [--detail]",
            "  epwshiftr storage validate [--query QUERY_ID] [--checksum]",
            "  epwshiftr storage repair [--execute]",
            "  epwshiftr storage cleanup [--scope SCOPE] [--older-than SECONDS] [--execute]"
        ),
        "storage layout" = c(
            "Usage: epwshiftr storage layout <show|set> [options]",
            "",
            "Show or update the download file layout policy."
        ),
        "storage layout show" = c(
            "Usage: epwshiftr storage layout show",
            "",
            "Show the current download file layout policy."
        ),
        "storage layout set" = c(
            "Usage: epwshiftr storage layout set --layout flat|dataset|drs|template [--template TEMPLATE]",
            "",
            "Update the download file layout policy."
        ),
        "storage report" = c(
            "Usage: epwshiftr storage report [--detail]",
            "",
            "Summarise download storage and cleanup candidates."
        ),
        "storage validate" = c(
            "Usage: epwshiftr storage validate [--query QUERY_ID] [--checksum]",
            "",
            "Validate store-managed NetCDF downloads against the manifest."
        ),
        "storage repair" = c(
            "Usage: epwshiftr storage repair [--execute]",
            "",
            "Preview or execute safe repairs for download file records."
        ),
        "storage cleanup" = c(
            "Usage: epwshiftr storage cleanup [--scope SCOPE] [--older-than SECONDS] [--execute]",
            "",
            "Preview or clean temporary files and safe orphaned records."
        )
    )
}
