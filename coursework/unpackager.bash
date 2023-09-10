GROUP_NAME="group14"
PATH_TO_PACKAGE="src/${GROUP_NAME}"


if [ -d "$PATH_TO_PACKAGE" ];
then
  echo "Removing 'package ${GROUP_NAME};' from java files"
  sed "1d" $PATH_TO_PACKAGE/Agent14.java | tee src/Agent14.java
  sed "1d" $PATH_TO_PACKAGE/BiddingStrategy.java | tee src/BiddingStrategy.java
  sed "1d" $PATH_TO_PACKAGE/OpponentModel.java | tee src/OpponentModel.java
  sed "1d" $PATH_TO_PACKAGE/UserModelEstimator.java | tee src/UserModelEstimator.java

  echo "Deleting folder ${PATH_TO_PACKAGE}"
  rm -r $PATH_TO_PACKAGE

  echo "Adding new files to git"
  git add src/*

else
  echo "Package ${PATH_TO_PACKAGE} does not exist you stupid baby!!"
fi