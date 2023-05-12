// print help messages or pipeline version number if prompted
def help_or_version(String version) {
    // show version number
    if (params.version) {
        println(
            """
            ==============================================
                    BFTX-NF Pipeline version ${version}
            ==============================================
            """.stripIndent()
        )
        System.exit(0)
    }
}

// clean up output directory
def clean_outDir() {
    outDir = file(params.outDir)
    outDir.deleteDir()
    outDir.mkdir()
}