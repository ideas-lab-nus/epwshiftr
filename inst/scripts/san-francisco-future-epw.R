# 旧金山未来气象：3 个 GCM × 4 个 SSP × 2 个未来时段。
# 第一部分：Belcher 原始 Morphing；第二部分：三种月尺度方法比较。
#
# 在 epwshiftr/pkg 目录运行，uvr 管理 R 和依赖，脚本加载当前源码：
# uvr run --with devtools --with checkmate --with data.table --with duckdb \
#     --with mirai --with RNetCDF --with S7 \
#     inst/scripts/san-francisco-future-epw.R
#
# 在上述命令末尾加「-- --check-config」只检查本地配置及基准 EPW；
# 加「-- --plan-only」查询 ESGF、选择模型和保存计划，但不生成未来 EPW。
# 已经安装当前开发版及依赖时，也可以在 R 中 source() 本脚本。

# ---- 0. 用户配置 -----------------------------------------------------------

output_root <- file.path(getwd(), "outputs", "san-francisco")
baseline_epw <- Sys.getenv("SF_BASELINE_EPW", "")  # 留空：查找本机文件，否则下载。
gcm_count <- 3L
scenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")
future_periods <- list(`2050s` = 2041:2060, `2080s` = 2071:2090)

# 该 TMY3 文件的头部标示资料期为 1973–2005；历史模型参考期采用相同范围。
# TMY3 是拼接的代表年，各月记录中的年份不是一个连续观测时段。
historical_years <- 1973:2005

args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% c("--check-config", "--plan-only")))
check_config <- "--check-config" %in% args
plan_only <- "--plan-only" %in% args

# 在仓库中使用当前开发代码，避免误用已安装的旧版公开 API。
if (file.exists("DESCRIPTION") && dir.exists("R") &&
    identical(unname(read.dcf("DESCRIPTION")[1L, "Package"]), "epwshiftr")) {
    if (!requireNamespace("pkgload", quietly = TRUE)) {
        stop("请使用脚本开头的 uvr 命令准备依赖。", call. = FALSE)
    }
    pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
} else {
    library(epwshiftr)
}

# 用显式月尺度构造器消除 epwshiftr 方法名在多个时间尺度上的歧义。
transforms <- list(
    monthly_transform("original_morphing"),  # Belcher 原始 Morphing。
    monthly_transform("bws_btws"),           # Eames 的 BWS + BTWS。
    monthly_transform("epwshiftr")          # 本包的增强月尺度方法。
)
reference <- historical_reference(historical_years)
control <- shift_control(
    strict = TRUE,
    allow_partial = FALSE,
    download = "auto",
    resume = TRUE,
    overwrite = FALSE
)
ui <- shift_ui(detail = "normal")

# 查找或下载固定站点的基准 EPW，并复制到本次工作目录中保存。
sf__prepare_epw <- function(path, directory) {
    name <- "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    destination <- file.path(directory, name)
    if (nzchar(path)) {
        path <- normalizePath(path.expand(path), mustWork = TRUE)
    } else if (file.exists(destination)) {
        path <- destination
    } else {
        candidates <- Sys.glob(file.path(c(
            "/Applications/EnergyPlus*", "C:/EnergyPlus*",
            "/usr/local/EnergyPlus*"
        ), "WeatherData", name))
        if (length(candidates)) path <- candidates[[1L]]
    }
    if (nzchar(path)) {
        if (!identical(normalizePath(path), normalizePath(destination, mustWork = FALSE))) {
            stopifnot(file.copy(path, destination, overwrite = TRUE))
        }
    } else {
        url <- paste0(
            "https://energyplus-weather.s3.amazonaws.com/",
            "north_and_central_america_wmo_region_4/USA/CA/",
            sub("[.]epw$", "", name), "/", name
        )
        # 完整下载后再发布，避免把中断下载当作可复用的基准文件。
        temporary <- tempfile(tmpdir = directory, fileext = ".epw")
        on.exit(unlink(temporary), add = TRUE)
        utils::download.file(url, temporary, mode = "wb", method = "libcurl")
        stopifnot(file.copy(temporary, destination, overwrite = TRUE))
    }
    header <- strsplit(readLines(destination, n = 1L), ",", fixed = TRUE)[[1L]]
    if (length(header) < 10L || header[[1L]] != "LOCATION" ||
        header[[6L]] != "724940") {
        stop("基准 EPW 必须是旧金山国际机场站（WMO 724940）。", call. = FALSE)
    }
    normalizePath(destination, winslash = "/", mustWork = TRUE)
}

# 保存机器可读的完整清单；即使某个子任务失败，已完成输出也保留在清单中。
sf__save_results <- function(batch, directory) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    cases <- shift_cases(batch)
    outputs <- shift_outputs(batch)
    diagnostics <- shift_diagnostics(batch)
    summary <- shift_summary(batch, weather = TRUE)
    data.table::fwrite(cases, file.path(directory, "cases.csv"))
    data.table::fwrite(outputs, file.path(directory, "outputs.csv"))
    data.table::fwrite(diagnostics, file.path(directory, "diagnostics.csv"))
    data.table::fwrite(summary, file.path(directory, "summary.csv"))
    shift_watch(batch, follow = FALSE, ui = shift_ui(detail = "detail"))
    cat("\n状态：", shift_status(batch), "；EPW 文件数：", nrow(outputs), "\n", sep = "")
    invisible(identical(shift_status(batch), "completed"))
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
output_root <- normalizePath(output_root, winslash = "/", mustWork = TRUE)
store <- file.path(output_root, "store")
epw <- sf__prepare_epw(baseline_epw, file.path(output_root, "input"))
print(shift_site(epw = epw))
cat("\nSSP：", paste(scenarios, collapse = ", "), "\n", sep = "")
print(future_periods)
cat("\n输出根目录：", output_root, "\n", sep = "")

if (check_config) {
    cat("\n本地配置检查完成；未查询或下载 CMIP6 数据。\n")
} else {
    # ---- 共同准备：锁定三种方法都能使用的 3 个 GCM -------------------------
    # 这里会联网查询完整的 File/年份覆盖，不会用缺少变量或情景的模型凑数。
    comparison_plan <- shift_future_epw(
        epw = epw,
        climate = shift_cmip6(model = gcm_count, scenarios = scenarios, member = "r1i1p1f1"),
        periods = future_periods,
        transform = transforms,
        reference = reference,
        dir = file.path(output_root, "02-three-methods"),
        store = store,
        control = control,
        ui = ui,
        dry_run = TRUE
    )
    selected <- unique(shift_cases(comparison_plan)[, c("model", "member", "grid"), with = FALSE])
    models <- unique(selected$model)
    stopifnot(length(models) == gcm_count)
    print(selected)
    data.table::fwrite(selected, file.path(output_root, "selected-gcms.csv"))

    # ---- 1. Belcher 原始 Morphing：3 GCM × 4 SSP × 2 时段 = 24 EPW -----------
    # 显式复用前面选定的三个 GCM；methods 保证返回可恢复的 ShiftBatch。
    belcher_plan <- shift_future_epw(
        epw = epw,
        climate = shift_cmip6(model = models, scenarios = scenarios, member = "r1i1p1f1"),
        periods = future_periods,
        methods = "original_morphing",
        reference = reference,
        dir = file.path(output_root, "01-belcher"),
        store = store,
        control = control,
        ui = ui,
        dry_run = TRUE
    )

    # 运行前记录批次 ID，进程中断后仍可重新打开，不依赖当前 R 会话中的对象。
    batches <- list(
        store = store,
        belcher = shift_ids(belcher_plan)$batch_id,
        comparison = shift_ids(comparison_plan)$batch_id,
        baseline_epw = epw,
        scenarios = scenarios,
        periods = future_periods,
        historical_years = historical_years,
        selected_gcms = selected
    )
    saveRDS(batches, file.path(output_root, "batches.rds"))
    writeLines(capture.output(sessionInfo()), file.path(output_root, "session-info.txt"))

    if (plan_only) {
        print(belcher_plan)
        print(comparison_plan)
        cat("\n两个计划和批次 ID 已保存；未生成未来 EPW。\n")
    } else {
        # 从解析/提取历史与未来数据到 Morphing、EPW 写出均由任务 API 执行。
        belcher <- shift_run(belcher_plan, ui = ui)
        belcher_ok <- sf__save_results(belcher, file.path(output_root, "reports-belcher"))

        # ---- 2. 三方法批量：3 方法 × 3 GCM × 4 SSP × 2 时段 = 72 EPW ---------
        # 使用已完成共同模型筛选的计划，三种方法共享模型/member/grid 身份。
        comparison <- shift_run(comparison_plan, ui = ui)
        comparison_ok <- sf__save_results(comparison, file.path(output_root, "reports-three-methods"))
        if (!belcher_ok || !comparison_ok) {
            stop("存在未完成子任务，请查看 diagnostics.csv 并使用下方恢复步骤。", call. = FALSE)
        }
        cat("\n两部分均已完成。请查看各 reports 目录的 outputs.csv 和 summary.csv。\n")
    }
}

# ---- 中断/失败后的恢复：在已加载当前 epwshiftr 的 R 会话中单独执行 ----------
# ids <- readRDS("outputs/san-francisco/batches.rds")
# belcher <- shift_batch_get(ids$belcher, store = ids$store)
# belcher <- shift_resume(belcher, ui = shift_ui())
# comparison <- shift_batch_get(ids$comparison, store = ids$store)
# comparison <- shift_resume(comparison, ui = shift_ui())
# shift_outputs(comparison)
# shift_diagnostics(comparison)
# shift_summary(comparison, weather = TRUE)
