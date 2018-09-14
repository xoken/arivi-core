def running(gitlabBuildName) {
	updateGitlabCommitStatus(name: "${gitlabBuildName}", state: 'running')
}

def success(gitlabBuildName) {
	updateGitlabCommitStatus(name: "${gitlabBuildName}", state: 'success')
}

def failure(gitlabBuildName) {
	updateGitlabCommitStatus(name: "${gitlabBuildName}", state: 'failed')
}
pipeline {
    agent any
    options {
        gitLabConnection('Gitlab');
    }
    stages {
        stage('Build') {
            steps {
                 gitlabBuilds(builds: ["1.Build", "2.Lint Checking"]) {
                    echo 'Building..'
                    running('1.Build');
                    sh 'stack clean'
                    sh 'stack build'

                }
            }
            post {
                success {
                    success('1.Build');

                }
                failure {
                    failure('1.Build');
                    failure('2.Lint Checking');
                }
            }
        }

        stage('Lint Checking') {
            steps {
                gitlabBuilds(builds: ["2.Lint Checking","3.Testing"]) {
                    echo 'Checking lint..'
                    running('2.Lint Checking');
                    sh 'hlint --extension=hs .'
                }
            }
            post {
                success {
                    success('2.Lint Checking');
                }
                failure {
                    failure('2.Lint Checking');
                    failure('3.Testing');
                }
            }
        }
        stage('Test') {
            steps {
                 gitlabBuilds(builds: ["3.Testing"]) {
                    echo 'Testing..'

                }
            }

            post {
                success {
                    success('3.Testing');
                }
                failure {
                    failure('3.Testing');
                }
            }

        }
       stage('Deploy') {
            when {
              expression {
                env.DEPLOY_BRANCH ==  env.GIT_BRANCH
              }
            }
            steps {
                gitlabBuilds(builds: ["Deploy"]) {
                    echo 'Deploying....'
                    running('4.Deploy');
                    sh 'mv `stack path --local-install-root`/bin/Main scripts/Deployment-Tools/Main'
                    sh 'chmod +x scripts/Deployment-Tools/cronejob.sh'
                    sh 'cd scripts/Deployment-Tools; python fabfile.py Main 180;rm Main'
                }
            }
            post {
                success {
                    success('4.Deploy');
                }
                failure {
                    failure('4.Deploy');
                }
            }
        }
    }

  post {
        success {
          updateGitlabCommitStatus name: 'build', state: 'success'
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
          updateGitlabCommitStatus name: 'build', state: 'failed'
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
