#!/usr/bin/env Rscript
# UpdateDoc.R —— 一键更新文档 & 本地检查
pkg_path <- dirname('./')   # 文件所在文件夹
if (!file.exists(file.path(pkg_path, "DESCRIPTION")))
  stop("DESCRIPTION not found! 请把本文件放在包根目录。")

setwd(pkg_path)                          # 确保工作目录正确
devtools::document()                     # 生成 Rd + NAMESPACE
#devtools::check()                        # 本地 R CMD check
#message("✅ 文档已更新，检查完成！")
message("✅ 文档已更新！")
