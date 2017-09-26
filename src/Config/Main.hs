module Config.Main where

import Task (sudo, createSimpleTask)

import qualified Module.File as File
import qualified Module.Copy as Copy
import qualified Module.Shell as Shell

copyConsulBinary = sudo $ createSimpleTask Copy.CopyModule {
    Copy.src = "./bin/consul",
    Copy.dest = "/usr/local/bin/consul",
    Copy.properties = Copy.InheritFromSrc
}

copyConsulService = createSimpleTask Copy.CopyModule {
    Copy.src = "./unit-files/consul.service",
    Copy.dest = "/etc/systemd/system/consul.service",
    Copy.properties = Copy.InheritFromSrc
}

reloadSystemctlUnits = sudo $ createSimpleTask $ Shell.shellExec "systemctl" ["daemon-reload"]
startConsulService = sudo $ createSimpleTask $ Shell.shellExec "systemctl" ["start", "consul"]

grr = sudo $ createSimpleTask $ Shell.shellExec "echo" ["wat"]
root = [grr]
