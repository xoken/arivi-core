import Arivi.Network.Instance
import qualified Data.Configurator as C

main = do
    let kconfig =
            Config
                { bootStrapPeers = []
                , k = 10
                , privateKey = "b59c537c6e9ff785e1fb7da59ca2e1512557d5c4559fc8be941eb9ec0f7240f3"
                , responseTime = 60000000
                , threadSleepTime = 30000000
                , logFilePath = "./log/node.log"
                }
    let aconfig = AriviConfig {hostip = "127.0.0.1", udpport = "7000", tcpPort = "7000"}
    ai <- getAriviInstance aconfig
    runAriviInstance ai kconfig
    print "Arivi is now running"
    return ()
