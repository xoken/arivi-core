from fabric import Connection
import pandas as pd
import sys

src = "./"
dst = "/home/xkn/fluentdRun/"
remoteConfigFile = "config1.yaml"
configFileName = "ServerConfig.xls"

def getHostNameList(configFileName):
    xls = pd.ExcelFile(configFileName)
    firstSheet = xls.parse(0)
    myIpColumn = firstSheet['myIp']
    hostUserNameList = []
    for (hostName) in  (myIpColumn):
        hostUserNameList.append(str(hostName))
    return hostUserNameList

def getRemoteUserNameList(configFileName):
    xls = pd.ExcelFile(configFileName)
    firstSheet = xls.parse(0)
    userNameColumn = firstSheet['userName']
    remoteUserNameList = []
    for (remoteUserName) in  (userNameColumn):
        remoteUserNameList.append(str(remoteUserName))
    return remoteUserNameList

def generateConfigFile(srcPath, dstPath, remoteUserName, serverIp, rowNo):
    print("Generating config file " + remoteConfigFile + " to " + \
             dst  + remoteConfigFile + " at " + serverIp)
    try:
        xls = pd.ExcelFile(configFileName)
        firstSheet = xls.parse(0)
        nthRow = firstSheet.loc[rowNo]
        (_, totalColumns) = firstSheet.shape
        columnNames = list(firstSheet.columns.values)
        configFile = "echo \""

        for i in range(totalColumns):
            try:
                configFile = configFile + str(columnNames[i]) \
                            + ": " + str(nthRow[i]) + "\n"
            except:
                try:
                    configFile = configFile + str(columnNames[i]) \
                                + ": " + nthRow[i] + "\n"
                except:
                    configFile = configFile + str(columnNames[i]) + ": " + "" + "\n"
        configFile = configFile + "\"" + " > " + dst + remoteConfigFile
        Connection(remoteUserName + "@" + serverIp).run(configFile)
        print("Configfile generation is done")
        print(configFile)
    except :
        print("Can not generate configFile at " + serverIp)


def copyFile(srcPath, dstPath, remoteUserName, serverIp, executableName):
    print("Deleting path if any " + dst + executableName + " at " + serverIp)

    try:
        result = Connection(remoteUserName + "@" + serverIp).run(\
                        "rm -rf " + dstPath + executableName)
        print("Deleted " + dstPath + executableName)
    except :
        print(executableName + "not found " + " to remote server")

    print("Copying " + src + executableName + " to " + dst + executableName + \
                                         " at " + serverIp)
    try:
        result = Connection(remoteUserName + "@" + serverIp).put(\
                        srcPath + executableName,dstPath + executableName)
        print("Uploaded {0.local} to {0.remote}".format(result))
    except :
        print("Can not copy " + executableName + " to remote server")

def killPrevious(dstPath,remoteUserName,serverIp,executableName):
    try:
        print("Killing previous executable  if any " + dstPath + executableName)
        Connection(remoteUserName + "@" + serverIp).run("pkill -f " + dstPath + \
                                                + executableName)
    except:
        print("No previous process to kill")

def executeProgram(dstPath,remoteUserName,serverIp,executableName):
    try:
        print("Making executable " + dstPath + executableName)
        Connection(remoteUserName + "@" + serverIp).run("chmod +x " \
                                        + dstPath + executableName)
    except:
        print("Can not make file executable")
    try:
        print("Executing " + dstPath + executableName + " at " + serverIp)
        executablePath = dstPath + executableName
        # Connection(remoteUserName + "@" + serverIp).run("nohup " \
        #         + executablePath +  " > nohup.out 2> nohup.err < /dev/null &")
        # Connection(remoteUserName + "@" + serverIp).run("exec 1>&2; " + executablePath + " & disown ")
        Connection(remoteUserName + "@" + serverIp).run(executablePath)
    except:
        print("Can not execute file")
def deploy(srcPath,dstPath,remoteUserName,serverIp,rowNo,executableName):
    #killPrevious(dstPath,remoteUserName,serverIp,executableName)
    copyFile(srcPath,dstPath,remoteUserName,serverIp,executableName)
    #generateConfigFile(srcPath,dstPath,remoteUserName,serverIp,rowNo)
    #executeProgram(dstPath,remoteUserName,serverIp,executableName)


def deployAll(configFileName):
    hostList = getHostNameList(configFileName)
    remoteUserNameList = getRemoteUserNameList(configFileName)
    rowNo = 0
    executableName = str(sys.argv[1])
    for (remoteUserName,serverIp) in zip(remoteUserNameList,hostList):
        deploy(src, dst, remoteUserName, serverIp, rowNo,executableName)
        rowNo = rowNo + 1
        print("Done for " + serverIp + "\n")

deployAll(configFileName)
