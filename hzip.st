#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"huffman zip utility"

"global vars"
nl := Character nl.
flag := nil.
filename := nil.
priority := SortedCollection new.

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]


Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
]

freqtable := ZeroArray new: 257.
"entry 256 will represent the EOF character"
freqtable at: 256 put: 1.

"Smalltalk arguments size printNl."
stdout << (Smalltalk arguments size) << nl.
stdout flush.

(Smalltalk arguments size < 2) | (Smalltalk arguments size > 3)
   ifTrue: [^self error: 'wrong number of arguments']
   ifFalse: [
      flag := Smalltalk arguments at: 1.
      "Parse flag later, maybe put each individual character into a set"
      stdout << flag << nl.
      stdout flush.
      filename := Smalltalk arguments at: 2.
      infile := FileStream open: filename
                           mode: FileStream read.
      [infile atEnd not] whileTrue: [
         |ordChar|
         ordChar := infile next asInteger.
         freqtable incr: ordChar.
      ].
      
      infile close.

      freqtable keysAndValuesDo: [:key :value |
         (value notNil and: [value > 0]) ifTrue: [
         stdout << 'freq[' << key asCharacter visible << ']=' << value
                << nl.
         ]
      ].
      
      
   ]


