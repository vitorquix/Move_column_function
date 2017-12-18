# Move_column_function

Function to move a column in R.

Description:

```r
########### Functions ###########
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}
```

How to use:

```r
df <- data.frame("Col1" = 1,"Col2" = 2,"Col3" = 3)

#Col3 before Col1
df <- df[moveme(names(df),"Col3 before Col1")]

#Col3 after Col1
df <- df[moveme(names(df),"Col3 after Col1")]

#Col3 first
df <- df[moveme(names(df),"Col3 first")]

#Col3 last
df <- df[moveme(names(df),"Col3 last")]
```
