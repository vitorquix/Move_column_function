# Move_column_function

Função para mover colunas em R.

Descrição:

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

Como usar:

```r
df <- data.frame("Col1" = 1,"Col2" = 2,"Col3" = 3)

#Col3 before Col2
df <- df[moveme(names(df),"Col3 before Col2")]

#Col1 after Col3
df <- df[moveme(names(df),"Col1 after Col3")]

#Col3 first
df <- df[moveme(names(df),"Col3 first")]

#Col1 last
df <- df[moveme(names(df),"Col1 last")]
```
