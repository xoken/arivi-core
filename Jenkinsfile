pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
                echo 'Building..'
                sh 'hlint --extension=hs .'
                sh 'stack build'                
                emailext (
                    subject: "STARTED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'",
                    body: """<p>STARTED: Job '${env.DEFAULT_RECIPIENTS} [${env.DEFAULT_REPLYTO}]':</p>
                      <p>Check console output at &QUOT;<a href='${env.ABC}'>${env.JOB_NAME} [${env.BUILD_NUMBER}]</a>&QUOT;</p>""",
                    to: "jenkins-notifications@xoken.org"
              )
            }
        }
        stage('Test') {
            steps {
                echo 'Testing..'
            }
        }
        stage('Deploy') {
            steps {
                echo 'Deploying....'
            }
        }
    }
    
  post {
        success {

          emailext (
              subject: "SUCCESSFUL: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'",
              body: """<p>'${env.PROJECT_NAME}' - Build #'${env.BUILD_NUMBER}' - '${env.BUILD_STATUS}':</p><p>SUCCESSFUL: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]':</p>
                <p>Check console output at &QUOT;<a href='${env.BUILD_URL}'>${env.JOB_NAME} [${env.BUILD_NUMBER}]</a>&QUOT;</p>""",
              to: "jenkins-notifications@xoken.org"
            )
        }

        failure {

          emailext (
              subject: "FAILED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'",
              body: """<p>'${env.PROJECT_NAME}' - Build #'${env.BUILD_NUMBER}' - '${env.BUILD_STATUS}':</p><p>FAILED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]':</p>
                <p>Check console output at &QUOT;<a href='${env.BUILD_URL}'>${env.JOB_NAME} [${env.BUILD_NUMBER}]</a>&QUOT;</p>""",
              to: "jenkins-notifications@xoken.org"
            )
        }
  }
}
