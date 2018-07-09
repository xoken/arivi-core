from fabric import *
import pandas as pd

# import generateConfigFile


src = "./"
# dst = "/root/hello.sh"
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

# def generateConfigFiles(configFileName):
#     xls = pd.ExcelFile(configFileName)
#     firstSheet = xls.parse(0)

#     userNameColumn = firstSheet['userName']
#     myIpColumn = firstSheet['myIp']
#     nodeTypeColumn = firstSheet['nodeType']
#     logFileColumn = firstSheet['logFile']
#     secretKeyColumn = firstSheet['secretKey']
#     trustedPeersColumn = firstSheet['trustedPeers']
#     udpPortColumn = firstSheet['udpPort']
#     tcpPortColumn = firstSheet['tcpPort']
#     myNodeIdColumn = firstSheet['myNodeId']

#     for (myIp,nodeType,logFile,secretKey,trustedPeers,udpPort,tcpPort,myNodeId) \
#      in zip(myIpColumn,nodeTypeColumn,logFileColumn,secretKeyColumn,\
#             trustedPeersColumn,udpPortColumn,tcpPortColumn,myNodeIdColumn):
#         pass
def generateConfigFile(srcPath, dstPath, remoteUserName, serverIp, rowNo):
    print("Generating config file " + remoteConfigFile + " to " + dst  + remoteConfigFile + " at " + serverIp)
    try:
        xls = pd.ExcelFile(configFileName)
        firstSheet = xls.parse(0)
        # headingRow = firstSheet.loc[0]
        nthRow = firstSheet.loc[rowNo]
        # print("Uploaded {0.local} to {0.remote}".format(result))
        # for cell in nthRow:
        (_, totalColumns) = firstSheet.shape
        columnNames = ["userName","myIp","nodeType"	,"logFile","secretKey","trustedPeers","udpPort","tcpPort","myNodeId"]
        configFile = "echo \""
        # print columnNames[0]
        # print columnNames[1]
        # print columnNames[2]
        # print columnNames[3]
        # print columnNames[4]
        # print columnNames[5]
        # print columnNames[6]
        # print columnNames[7]
        # print columnNames[8]
        # print columnNames[9]
        # print (nthRow[0]).isnull()
        for i in range(totalColumns):
            try:
                # print i + " " + nthRow[i]
                configFile = configFile + columnNames[i]  + ": " + str(nthRow[i]) + "\n"
            except:
                configFile = configFile + columnNames[i]  + ": " + "" + "\n"
                # print "Can't generate config file"
        # if nthRow[0]:
        #     configFile = configFile + "userName: " + nthRow[0] + "\n"
        # else:
        #     configFile = configFile + "userName: " + "" + "\n"

        # if nthRow[1]:
        #     configFile = configFile + "myIp: " + nthRow[1] + "\n"
        # else:
        #     configFile = configFile + "myIp: " + "" + "\n"

        # if nthRow[2]:
        #     configFile = configFile + "nodeType: " + nthRow[2] + "\n"
        # else:
        #     configFile = configFile + "nodeType: " + "" + "\n"

        # if nthRow[3]:
        #     configFile = configFile + "logFile: " + nthRow[3] + "\n"
        # else:
        #     configFile = configFile + "logFile: " + "" + "\n"

        # if nthRow[4]:
        #     configFile = configFile + "secretKey: " + nthRow[4] + "\n"
        # else:
        #     configFile = configFile + "secretKey: " + "" + "\n"

        # if nthRow[5]:
        #     configFile = configFile + "trustedPeers: " + nthRow[5] + "\n"
        # else:
        #     configFile = configFile + "trustedPeers: " + "" + "\n"

        # if nthRow[6]:
        #     configFile = configFile + "udpPort: " + nthRow[6] + "\n"
        # else:
        #     configFile = configFile + "udpPort: " + "" + "\n"

        # if nthRow[7]:
        #     configFile = configFile + "tcpPort: " + nthRow[7] + "\n"
        # else:
        #     configFile = configFile + "tcpPort: " + "" + "\n"

        # if nthRow[8]:
        #     configFile = configFile + "myNodeId: " + nthRow[8] + "\n"
        # else:
        #     configFile = configFile + "myNodeId: " + "" + "\n"
        configFile = configFile + "\"" + " > " + dst + remoteConfigFile
        result = Connection(remoteUserName + "@" + serverIp).run(configFile)
        # print cell
        # print configFile
        print "Configfile generation is done"
    except :
        print("Can not connect to remote server")


def copyFile(srcPath,dstPath,remoteUserName,serverIp):
    print("Copying " + src + executableName + " to " + dst + executableName +  " at " + serverIp)
    try:
        result = Connection(remoteUserName + "@" + serverIp).put(srcPath + executableName,dstPath + executableName)
        print("Uploaded {0.local} to {0.remote}".format(result))
    except :
        print("Can not connect to remote server")

def executeProgram(dstPath,remoteUserName,serverIp):
    try:
        print("Making executable " + dstPath + executableName)
        result = Connection(remoteUserName + "@" + serverIp).run("chmod +x " + dstPath + executableName)
    except:
        print("Can not make file executable")
    try:
        print("Executing " + dstPath + remoteConfigFile + " at " + serverIp)
        result = Connection(remoteUserName + "@" + serverIp).run(dstPath + executableName)
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
