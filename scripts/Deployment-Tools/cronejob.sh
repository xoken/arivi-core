# source /home/jenkins/ssh-agent.cf
if [ `find /home/xkn/fluentdRun/Main -mmin -60` ]; then
    # if pgrep '/home/xkn/Main .' 2>/dev/null; then
    #     # kill -9 $(ps aux | grep '/home/xkn/Main .' | awk '{print $2}') 2 > /dev/null
    #     echo "Terminating /home/xkn/Main" >> /home/xkn/Main.log
    #     pkill -f "/home/xkn/Main ."
    # fi
    pkill -f "/home/xkn/fluentdRun/Main ."
    nohup bash `/home/xkn/fluentdRun/Main .` > nohup.out 2> nohup.err < /dev/null &
fi