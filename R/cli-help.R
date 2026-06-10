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
        "Usage: epwshiftr [--store PATH] [--json] [--quiet] <group> <command> [options]",
        "",
        "Command groups:",
        "  epwshiftr doctor",
        "  epwshiftr query help",
        "  epwshiftr download help",
        "  epwshiftr workflow help",
        "  epwshiftr storage help",
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
            "  epwshiftr query search [--index-node URL] [--type Dataset|File|Aggregation] [--dry-run] key=value ...",
            "  epwshiftr query add --query-file PATH [--label LABEL] [--track]",
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
            "Usage: epwshiftr query search [--index-node URL] [--type Dataset|File|Aggregation] [--fields FIELDS] [--limit N] [--all] [--dry-run] key=value ...",
            "",
            "Run an ESGF search from command-line key=value constraints.",
            "Use comma-separated values for multi-value query parameters."
        ),
        "query add" = c(
            "Usage: epwshiftr query add --query-file PATH [--label LABEL] [--track]",
            "",
            "Import an EsgQuery JSON file into the selected store."
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
        download = c(
            "Usage: epwshiftr download <command> [options]",
            "",
            "Commands:",
            "  epwshiftr download preflight <query_id> [--replica POLICY] [--strategy STRATEGY] [--no-probe]",
            "  epwshiftr download run <query_id> [--session-label LABEL] [--overwrite] [--no-resume] [--no-progress]",
            "  epwshiftr download status [--query QUERY_ID] [--session SESSION_ID]",
            "  epwshiftr download sessions",
            "  epwshiftr download tasks [--session SESSION_ID] [--status STATUS]",
            "  epwshiftr download events [--session SESSION_ID] [--task TASK_ID]",
            "  epwshiftr download watch [--query QUERY_ID] [--session SESSION_ID] [--events N]",
            "  epwshiftr download logs [--session SESSION_ID] [--task TASK_ID] [--tail N]",
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
            "Usage: epwshiftr download run <query_id> [--session-label LABEL] [--overwrite] [--no-resume] [--no-progress]",
            "",
            "Refresh, plan, enqueue, and run downloads for a stored query."
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
            "Usage: epwshiftr download tasks [--session SESSION_ID] [--status STATUS]",
            "",
            "List persistent download tasks."
        ),
        "download events" = c(
            "Usage: epwshiftr download events [--session SESSION_ID] [--task TASK_ID]",
            "",
            "List persistent downloader event records."
        ),
        "download watch" = c(
            "Usage: epwshiftr download watch [--query QUERY_ID] [--session SESSION_ID] [--events N]",
            "",
            "Return a one-shot download activity snapshot with summary, tasks, nodes, and recent events."
        ),
        "download logs" = c(
            "Usage: epwshiftr download logs [--session SESSION_ID] [--task TASK_ID] [--tail N]",
            "",
            "Return the latest persistent downloader event records."
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
        workflow = c(
            "Usage: epwshiftr workflow <command> [options]",
            "",
            "Commands:",
            "  epwshiftr workflow report [--query QUERY_ID]"
        ),
        "workflow report" = c(
            "Usage: epwshiftr workflow report [--query QUERY_ID]",
            "",
            "Return a compact ESGF query workflow health report."
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
