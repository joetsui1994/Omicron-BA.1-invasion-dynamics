@Grab('org.yaml:snakeyaml:1.17')
import org.yaml.snakeyaml.Yaml
include { display_version } from './messages'

// print help messages or pipeline version number if prompted
def help_or_version(String version) {
    // show version number
    if (params.version) {
        display_version(version)
        System.exit(0)
    }
}

// write params to params_summary.config file in outDir
def export_params_summary() {
    // write config summary to file in outDir
    params_config = file("$params.outDir/params.config")
    params_config.text = "// params.config\n\nparams {\n"

    // loop over params
    for (param in params) {
        params_config.append("\t$param\n")
    }
    params_config.append("}")
}

// write params to params.yaml file in outDir
def export_params_yaml() {
    // write config summary to YAML file in outDir
    params_yaml = file("$params.outDir/params.yaml")
    params_yaml.text = "---\n"

    // loop over params
    for (param in params) {
        params_yaml.append("  $param.key: $param.value\n")
    }
}

// generate beastgen-options from specified YAML file
def parse_beastgen_yaml(String key, yaml_file) {
    // parse YAML file with snakeyaml
    Yaml parser = new Yaml()
    Map beastgen_params = parser.load((yaml_file as File).text)
    
    // collect global params that apply to all runs
    def params_map = [:]
    beastgen_params.each { k, v ->
        if (k != "runs") params_map.put(k, v)
    }

    // collect run-specific params and allow overwriting of global params if specified
    beastgen_run = beastgen_params.runs.find { it.key == key }
    beastgen_run.each { k, v ->
        if (k != "key") params_map.put(k, v)
    }

    // print params as string for input into BEASTGen
    beastgen_str = params_map.collect { k, v -> "$k=${v.toString().trim()}" }.join(",")

    println("$beastgen_str")
    // }
}

// merge global (or default) params with run-specific params
def merge_params(Map map_left, Map map_right) {
    return map_right.inject(map_left.clone()) { map, entry ->
        if (map[entry.key] instanceof Map && entry.value instanceof Map) {
            map[entry.key] = merge_params(map[entry.key], entry.value)
        } else {
            map[entry.key] = entry.value
        }
        return map
    }
}