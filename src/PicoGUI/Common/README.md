# What would a good UI definition look like?

```Haskell
row >>
    mainMenu [Fluid]
<>
row >>
    col [Fixed, "800px"] >> 
        menuItem "New App" (onClick doNewApp)
        <>
        menuItem "Statistics" (onClick showStats)
    <> -- same level unite
    col [Fluid, CenterAlign] >> -- going down a level
        header "Please login: " ==> -- creating an on the fly component with shared state
            inputText reactOn userName <> buttonOk (onClick $ processLogin userName) 
```

So, the idea is - an abstract IDL. Position / size data should be separated from visuals data. Then it's a composition into list / multi-tree structure.