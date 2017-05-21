#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"huffman zip utility"

"global vars"
nl := Character nl.
flag := nil.
priority := SortedCollection new.
bitArray := Array new.


FileStream extend [
   |bitIndex currentChar|

   nextBit [
      |bit|
      bitIndex isNil ifTrue: [bitIndex := 0].
      bitIndex = 0 ifTrue: [
         bitIndex := 8.
         currentChar := self next.
      ].
      bit := currentChar value bitAt: bitIndex.
      bitIndex := bitIndex - 1.
      ^ bit
   ]

   atBitEnd [
      ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
   ]

]

Object subclass: Stack [
   |stack topIdx|
   
   Stack class >> new [
      |result|
      result := super new.
      result init.
      ^result
   ]

   init [
      stack := Array new: 256.
      topIdx := 1.
   ]

   push: item [
      stack at: topIdx put: item.
      topIdx := topIdx + 1.
      topIdx = 257 ifTrue: ['error in stack' printNl.].
   ]

   pop [
      |tempIdx|
      topIdx < 1 
      ifTrue: [^nil]
      ifFalse: [
         topIdx := topIdx - 1.
         ^ (stack at: topIdx)
      ]
   ]
   
   isEmpty [
      ^ (topIdx = 2)
   ]
]

Object subclass: OutBits [
      stack := Array new: 256.
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

writeBitArray := [:outfile|
   |outBytes|
   outBytes := OutBits new: outfile.
   bitArray do: [:bit| outBytes writeBit: bit].
   outBytes flushByte.
].


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

   postorder: bitArr [
      |bitIdx tempArr tempBit|

      bitIdx := 8.
      tempArr := bitArr.
      tempArr := tempArr, #(0).
      [bitIdx = 0] whileFalse: [
         tempBit := Array new: 1.
         tempBit at: 1 put: (char bitAt: bitIdx).
         tempArr := tempArr, tempBit.
         bitIdx := bitIdx - 1.
      ].
      char = 0 ifTrue: [
         tempArr := tempArr, #(0).
      ].
      char = 256 ifTrue: [
         tempArr := tempArr, #(1).
      ].
      ^ tempArr
   ]

 
]

Leaf subclass: Tree [
   |left right|

   left [^ left]

   right [^ right]

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

   postorder: bitArr [
      |tempArr|
      tempArr := bitArr.
      tempArr := tempArr, (left postorder: bitArr).
      tempArr := tempArr, (right postorder: bitArr).
      tempArr := tempArr, #(1).
      ^ tempArr
   ]
]


"Smalltalk arguments size printNl."
stdout << (Smalltalk arguments size) << nl.
stdout flush.

freqtable := ZeroArray new: 257.
"entry 256 will represent the EOF character"
freqtable at: 256 put: 1.

encodeTable := ZeroArray new: 257.

(Smalltalk arguments size < 2) | (Smalltalk arguments size > 3)
   ifTrue: [^self error: 'wrong number of arguments']
   ifFalse: [
      |outFilename inFilename hufTree|
      outFilename := nil.
      inFilename := nil.
      flag := Smalltalk arguments at: 1.
      "Parse flag later, maybe put each individual character into a set"
      flagSet := Set new.
      i := 2.
      [i = ((flag size) + 1)] whileFalse: [
         flagSet add: (flag at: i).
         i := i + 1.
      ].
      
      inFilename := Smalltalk arguments at: 2.
      (Smalltalk arguments size = 3) ifTrue: [
         outFilename := Smalltalk arguments at: 3.
      ].
      
      infile := FileStream open: inFilename
                           mode: FileStream read.


      (flagSet includes: $t) | (flagSet includes: $c) 
      ifTrue: [
         "if the flag contains t or c, go to encoding."
         [infile atEnd not] whileTrue: [
            |ordChar|
            ordChar := infile next asInteger.
            freqtable incr: ordChar.
         ].
         
         infile close.
         "format this "
         'freq table' printNl.

         freqtable keysAndValuesDo: [:key :value |
            (value notNil and: [value > 0]) ifTrue: [
            stdout << 'freq[' << key asCharacter visible << ']=' << value
                   << nl.
            ]
         ].   

         freqtable keysAndValuesDo: [:key :value |
            (value notNil and: [value > 0]) ifTrue: [
               |tempLeaf|
               tempLeaf := Leaf new: key count: value.
               priority add: tempLeaf. 
            ]
         ].
           
         [priority notEmpty] whileTrue: [
            tLeft := priority removeFirst.
            tRight := priority removeFirst.
            tTree := Tree new: (tLeft char) count: ((tLeft count) + (tRight count)) 
                                                 left: tLeft right: tRight.
            priority notEmpty 
            ifTrue: [
               priority add: tTree.
            ]
            ifFalse: [
               hufTree := tTree.
            ]
         ].
         
         hufTree inorder: [:chr :str | encodeTable at: chr put: str.]
                           prefix: ''.



         'enc huf tree' printNl.
         bitArray := hufTree postorder: bitArray.

         bitArray := bitArray, #(1).

         (flagSet includes: $t) 
         ifTrue: [
            "iterate over encode table from 0 to size(257)-1"
            currentIdx := 0.
            "stdout <<'encTable size' << (encodeTable size) << nl.
            stdout flush."
            0 to: ((encodeTable size) - 1) do: [:a|
               (encodeTable at: currentIdx) = nil ifFalse: [
                  "print a char if the currentIdx is a printable
                   character. otherwise print the currentIdx
                   as an integer"
                  charToPrint := nil.
                  (currentIdx < 33) | (currentIdx > 126) 
                  ifTrue: [
                     charToPrint := currentIdx asString.
                     stdout << '' << charToPrint << nl.
                  ]
                  ifFalse: [
                     charToPrint := (currentIdx asCharacter) asString.
                      stdout << '' << charToPrint << nl.
                  ]
               ].
               currentIdx := currentIdx + 1.
            ].
            stdout flush. 
            'to implement' printNl.
         ].


         (flagSet includes: $c) 
         ifTrue: [
    
            infile := FileStream open: inFilename 
                                 mode: FileStream read.

            'post order done' printNl.
            [infile atEnd not] whileTrue: [
               |ordChar tempArr tempStr tempCharArr|
               tempArr := Array new.
               ordChar := infile next asInteger.
               tempStr := encodeTable at: ordChar.
               tempStr do: [:c | 
                  tempCharArr := Array new: 1.
                  tempCharArr at: 1 put: ((c asString) asInteger).
                  tempArr := tempArr, tempCharArr.                 
               ].
               bitArray := bitArray, tempArr.
            ].
            
            eofEncArr := Array new.
            eofEncStr := encodeTable at: 256.
            eofEncStr do: [:c |
               eofTempArr := Array new: 1.
               eofTempArr at: 1 put: ((c asString) asInteger).
               eofEncArr := eofEncArr, eofTempArr.
            ]. 

            bitArray := bitArray, eofEncArr.

            paddingZeros := (8 - ((bitArray size) \\ 8)) \\ 8.

            bitArray size printNl.
            
            paddingZeros printNl.
            [paddingZeros = 0] whileFalse: [
               bitArray := bitArray, #(0).
               paddingZeros := paddingZeros - 1.    
            ].

            bitArray size printNl.
            
            infile close.

            outFilename = nil
               ifTrue: [writeBitArray value: stdout]
               ifFalse: [
               [  |outfile|
                  outfile := FileStream open: outFilename mode: FileStream write.
                  writeBitArray value: outfile.
                  outfile close.
               ] on: SystemExceptions.FileError do: [:signal |
                  |errno|
                  errno := File errno.
                  stdout flush.
                  stderr << execname << ': ' << filename << ': '
                         << signal messageText << nl.
                  stderr flush.
               ]
            ]

         ].

      ].


      (flagSet includes: $u)
      ifTrue: [
         |leStack leTree tempTreePtr|
         leStack := Stack new. 
         inputArray := Array new.
         byteArray := Array new: 8.
         count := 0.
         [infile atBitEnd not] whileTrue: [
            count := count + 1.
            byteArray at: count  put: (infile nextBit).
            count = 8 ifTrue: [
               inputArray := inputArray, byteArray.
               count := 0.
            ].
         ].
         (inputArray size) printNl.
         
         inputArrIdx := 1.
         stayInLoop := true.
         [stayInLoop] whileTrue: [
            (inputArray at: inputArrIdx) = 0 
            ifTrue: [
               inputArrIdx := inputArrIdx + 1.
               accum := 0.
               "charBinaryArray := ZeroArray new: 8."
               0 to: 7 do: [:a |
                  "charBinaryArray at: a put: 
                        (inputArray at: inputArrIdx).
                  inputArrIdx := inputArrIdx + 1."
                  accum := accum + ((inputArray at: inputArrIdx) * 
                                          (2 raisedTo: (7 - a))).
                  inputArrIdx := inputArrIdx + 1.
               ].
               accum = 0 
               ifTrue: [
                  (inputArray at: inputArrIdx) = 1
                  ifTrue: [
                     accum := accum + 256.
                  ].
                  inputArrIdx := inputArrIdx + 1.
               ].
               
               teaLeaf := Leaf new: accum count: 0.
               leStack push: teaLeaf.
            ]
            ifFalse: [
               inputArrIdx := inputArrIdx + 1.
               (leStack isEmpty) ifTrue: [
                  leTree := leStack pop.
                  stayInLoop := false.
               ]
               ifFalse: [
                  rChild := leStack pop.
                  lChild := leStack pop.
                  "'right child: ' printNl.
                  rChild inspect.
                  'left child: ' printNl.
                  lChild inspect."
                  teaTree := Tree new: 0 count: 0 
                                    left: lChild right: rChild.
                  leStack push: teaTree.                  
               ]
            ]
         ].
        
         leTree inorder: [:char :string |
            stdout << '[' << char<< ']' << string << nl.
         ] prefix: ''.
   
         

         tempTreePtr := leTree.
                 
         infile close.
  
         outFileFD := nil.

         outFilename = nil
         ifTrue: [
            outFileFD := stdout.
         ].
         outFilename = nil
         ifFalse: [
            outFileFD := FileStream open: outFilename 
                                    mode: FileStream write.
        ].

         haveSeenEOF := false.
         inputArrIdx to: (inputArray size) do: [:a |
            ((inputArray at: a) = 0)
            ifTrue: [
               ((tempTreePtr left) isMemberOf: Leaf) 
               ifTrue:[
                  tempTreeLeaf := (tempTreePtr left).
                  ((tempTreeLeaf char) = 256) 
                  ifTrue: [
                     haveSeenEOF := true.
                  ]
                  ifFalse:[
                     haveSeenEOF ifFalse: [
                        "outFileFD << (tempTreeLeaf char) asCharacter."
                        outFileFD nextPutByte: (tempTreeLeaf char).
                        tempTreePtr := leTree.
                     ].
                  ]
               ]
               ifFalse: [
                  tempTreePtr:= (tempTreePtr left).
               ]
            ]
            "if not zero"
            ifFalse: [
               ((tempTreePtr right) isMemberOf: Leaf)
               ifTrue: [
                  tempTreeLeaf := (tempTreePtr right).
                  ((tempTreeLeaf char) = 256)
                  ifTrue: [
                     haveSeenEOF := true.
                  ]
                  ifFalse: [
                     haveSeenEOF ifFalse: [
                        "outFileFD << (tempTreeLeaf char) asCharacter."
                        outFileFD nextPutByte: (tempTreeLeaf char).
                        tempTreePtr := leTree.
                     ].
                  ].
               ]
               ifFalse: [
                  tempTreePtr := (tempTreePtr right)
               ]
            ] 
         ].

         outFilename = nil
         ifTrue: [
            stdout flush.
         ]
         ifFalse: [
            outFileFD close.
         ]

      ].

   ]


