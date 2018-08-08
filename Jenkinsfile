pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
                echo 'Building..'
                sh 'stack clean'
                sh 'stack build'
            }
        }
        stage('Lint Checking') {
            steps {
                echo 'Checking lint..'
                sh 'hlint --extension=hs .'
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
                sh 'mv `stack path --local-install-root`/bin/Main scripts/Deployment-Tools/Main'
                sh 'chmod +x scripts/Deployment-Tools/cronejob.sh'
                sh 'cd scripts/Deployment-Tools; python fabfile.py Main 180;rm Main'
            }
        }
    }

  post {
        success {

          emailext (
              subject: "SUCCESSFUL: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'",
              body: """
              <p> BUILD_NUMBER : '${env.BUILD_NUMBER}' </p>
              <p> BUILD_ID : '${env.BUILD_ID}' </p>
              <p> BUILD_URL : '${env.BUILD_URL}' </p>
              <p> NODE_NAME : '${env.NODE_NAME}' </p>
              <p> JOB_NAME : '${env.JOB_NAME}' </p>
              <p> BUILD_TAG : '${env.BUILD_TAG}' </p>
              <p> JENKINS_URL : '${env.JENKINS_URL}' </p>
              <p> EXECUTOR_NUMBER : '${env.EXECUTOR_NUMBER}' </p>
              <p> JAVA_HOME : '${env.JAVA_HOME}' </p>
              <p> WORKSPACE : '${env.WORKSPACE}' </p>
              <p> SVN_REVISION : '${env.SVN_REVISION}' </p>
              <p> CVS_BRANCH : '${env.CVS_BRANCH}' </p>
              <p> GIT_COMMIT : '${env.GIT_COMMIT}' </p>
              <p> GIT_URL : '${env.GIT_URL}' </p>
              <p> GIT_BRANCH : '${env.GIT_BRANCH}' </p>
              <p> PROMOTED_URL : '${env.PROMOTED_URL}' </p>
              <p> PROMOTED_JOB_NAME : '${env.PROMOTED_JOB_NAME}' </p>
              <p> PROMOTED_NUMBER : '${env.PROMOTED_NUMBER}' </p>
              <p> PROMOTED_ID : '${env.PROMOTED_ID}' </p>
              <p> BUILD_URL : '${env.BUILD_URL}' </p>
              <p> JOB_NAME : '${env.JOB_NAME}' </p>
              <p> BUILD_NUMBER : '${env.BUILD_NUMBER}' </p>
                """,
              to: "xokensjenkins@flockgroups.com"
            )
        }

        failure {

          emailext (
              subject: "FAILED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'",
             body: """
              <p> BUILD_NUMBER : '${env.BUILD_NUMBER}' </p>
              <p> BUILD_ID : '${env.BUILD_ID}' </p>
              <p> BUILD_URL : '${env.BUILD_URL}' </p>
              <p> NODE_NAME : '${env.NODE_NAME}' </p>
              <p> JOB_NAME : '${env.JOB_NAME}' </p>
              <p> BUILD_TAG : '${env.BUILD_TAG}' </p>
              <p> JENKINS_URL : '${env.JENKINS_URL}' </p>
              <p> EXECUTOR_NUMBER : '${env.EXECUTOR_NUMBER}' </p>
              <p> JAVA_HOME : '${env.JAVA_HOME}' </p>
              <p> WORKSPACE : '${env.WORKSPACE}' </p>
              <p> SVN_REVISION : '${env.SVN_REVISION}' </p>
              <p> CVS_BRANCH : '${env.CVS_BRANCH}' </p>
              <p> GIT_COMMIT : '${env.GIT_COMMIT}' </p>
              <p> GIT_URL : '${env.GIT_URL}' </p>
              <p> GIT_BRANCH : '${env.GIT_BRANCH}' </p>
              <p> PROMOTED_URL : '${env.PROMOTED_URL}' </p>
              <p> PROMOTED_JOB_NAME : '${env.PROMOTED_JOB_NAME}' </p>
              <p> PROMOTED_NUMBER : '${env.PROMOTED_NUMBER}' </p>
              <p> PROMOTED_ID : '${env.PROMOTED_ID}' </p>
              <p> BUILD_URL : '${env.BUILD_URL}' </p>
              <p> JOB_NAME : '${env.JOB_NAME}' </p>
              <p> BUILD_NUMBER : '${env.BUILD_NUMBER}' </p>
                """,
              to: "xokensjenkins@flockgroups.com"
            )
        }
  }
}
