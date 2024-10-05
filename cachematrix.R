## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # 逆行列を格納するための変数
  
  # 行列の設定
  set <- function(y) {
    x <<- y     # 与えられた行列yをxに保存
    inv <<- NULL  # 行列が変更されたら逆行列をリセット
  }
  
  # 行列の取得
  get <- function() x
  
  # 逆行列のキャッシュに設定
  setInverse <- function(inverse) inv <<- inverse
  
  # 逆行列のキャッシュを取得
  getInverse <- function() inv
  
  # 関数のリストを返す
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve関数：行列の逆行列を計算し、キャッシュを利用する
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # まずキャッシュされた逆行列があるか確認
  
  # もしキャッシュが存在すればそれを返す
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # 逆行列がキャッシュされていない場合、新たに計算する
  data <- x$get()        # 行列を取得
  inv <- solve(data, ...)  # 逆行列を計算
  x$setInverse(inv)      # 計算結果をキャッシュに保存
  
  inv  # 計算された逆行列を返す
}


# 2x2の行列を作成
myMatrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))

# 逆行列を計算
cacheSolve(myMatrix)

# キャッシュを利用して再度逆行列を取得（再計算はされない）
cacheSolve(myMatrix)
