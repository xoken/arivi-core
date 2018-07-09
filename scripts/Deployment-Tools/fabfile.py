from fabric import Connection
import pandas as pd

src = "./"

dst = "/home/xkn/"
remoteConfigFile = "config.yaml"
executableName = "hello.sh"
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
                configFile = configFile + str(columnNames[i]) + ": " + "" + "\n"
        configFile = configFile + "\"" + " > " + dst + remoteConfigFile
        Connection(remoteUserName + "@" + serverIp).run(configFile)
        print "Configfile generation is done"
    except :
        print("Can not connect to remote server")


def copyFile(srcPath,dstPath,remoteUserName,serverIp):
    print("Copying " + src + executableName + " to " + dst + executableName + \
                                         " at " + serverIp)
    try:
        result = Connection(remoteUserName + "@" + serverIp).put(\
                        srcPath + executableName,dstPath + executableName)
        print("Uploaded {0.local} to {0.remote}".format(result))
    except :
        print("Can not connect to remote server")

def executeProgram(dstPath,remoteUserName,serverIp):
    try:
        print("Making executable " + dstPath + executableName)
        Connection(remoteUserName + "@" + serverIp).run("chmod +x " \
                                        + dstPath + executableName)
    except:
        print("Can not make file executable")
    try:
        print("Executing " + dstPath + remoteConfigFile + " at " + serverIp)
        Connection(remoteUserName + "@" + serverIp).run(dstPath\
                                                         + executableName)
    except:
        print("Can not execute file")

def deploy(srcPath,dstPath,remoteUserName,serverIp,rowNo):
    copyFile(srcPath,dstPath,remoteUserName,serverIp)
    generateConfigFile(srcPath,dstPath,remoteUserName,serverIp,rowNo)
    executeProgram(dstPath,remoteUserName,serverIp)


def deployAll(configFileName):
    hostList = getHostNameList(configFileName)
    remoteUserNameList = getRemoteUserNameList(configFileName)
    rowNo = 0
    for (remoteUserName,serverIp) in zip(remoteUserNameList,hostList):
        deploy(src, dst, remoteUserName, serverIp, rowNo)
        rowNo = rowNo + 1
        print("Done for " + serverIp + "\n")

deployAll(configFileName)
