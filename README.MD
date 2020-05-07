## PureScript Notes
  This is just a minor accumulation of Purescript Notes and Code samples


- *Data Types* - Expressions when evaluated reduce to values. Every value has a *Type*
- *Data Declarations* are how types are defined.
- *Type Constructor* - Is the name of the type and is capitalized
- *Data Constructors* - are the values that inhabit the type they are defined in.

### The definition of type Boolean
  ```haskell
    data Boolean = false | true
    --    [1]      [2]  [3]  [4]
  ```
  1. Type constructor for datatype `Boolean`
  2. Data constructor for value `false`
  3. Pipe - this indicates a sum type of logical disjunction. So a Boolean value is either true or false.
  4. Data constructor for value `true`

### Pattern Matching 
- Functions which return a value for any combination of inputs are called `Total` functions.
- Functions which don't return a value for any combination of inputs are called `Partial` functions.
- 