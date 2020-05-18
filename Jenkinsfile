pipeline {
  agent any
  stages {
    stage('Pull') {
      steps {
        git(url: 'https://github.com/xoken/arivi-core', branch: 'master')
      }
    }

    stage('Build') {
      steps {
        sh '''stack clean
stack install'''
      }
    }

  }
}