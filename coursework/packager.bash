GROUP_NAME="group14"
PATH_TO_PACKAGE="src/${GROUP_NAME}"
PACKAGE_DECLARATION="package ${GROUP_NAME};"

if [ ! -d "src/${GROUP_NAME}" ];
then
  echo "Creating package ${PATH_TO_PACKAGE}"
  mkdir -p "$PATH_TO_PACKAGE" && echo "Created package: ${PATH_TO_PACKAGE}"

  echo "Moving java files to ${PATH_TO_PACKAGE}"
  mv src/Agent14.java src/BiddingStrategy.java src/OpponentModel.java src/UserModelEstimator.java "$PATH_TO_PACKAGE"

  echo "Adding package declarations"
  echo $PACKAGE_DECLARATION | cat - $PATH_TO_PACKAGE/Agent14.java > temp && mv temp $PATH_TO_PACKAGE/Agent14.java
  echo $PACKAGE_DECLARATION | cat - $PATH_TO_PACKAGE/BiddingStrategy.java > temp && mv temp $PATH_TO_PACKAGE/BiddingStrategy.java
  echo $PACKAGE_DECLARATION | cat - $PATH_TO_PACKAGE/OpponentModel.java > temp && mv temp $PATH_TO_PACKAGE/OpponentModel.java
  echo $PACKAGE_DECLARATION | cat - $PATH_TO_PACKAGE/UserModelEstimator.java > temp && mv temp $PATH_TO_PACKAGE/UserModelEstimator.java

  echo "Adding new files to git"
  git add $PATH_TO_PACKAGE/*s
else
  echo "Package src/${PATH_TO_PACKAGE} already exists - please kill yourself"
fi