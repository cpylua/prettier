A prettier printer
==============

[A prettier printer](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf) by Philip Wadler

DEMO: JSON prettify
---------

	ghci> putStrLn . pretty 10 $ renderJValue json
	{
	  "a": 12.0,
	  "b": false,
	  "c": "Foo bar hello world",
	  "d": [
		1.0,
		2.0,
		3.0,
		4.0,
		5.0,
		6.0,
		7.0,
		8.0,
		9.0,
		10.0
	  ],
	  "long key": {
		"x": 100.0,
		"y": true,
		"z": [
		  100.0,
		  101.0,
		  102.0,
		  103.0,
		  104.0,
		  105.0,
		  106.0,
		  107.0,
		  108.0,
		  109.0,
		  110.0
		]
	  }
	}

	ghci> putStrLn . pretty 120 $ renderJValue json
	{
	  "a": 12.0, "b": false, "c": "Foo bar hello world", "d": [
		1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0
	  ], "long key": {
		"x": 100.0, "y": true, "z": [
		  100.0, 101.0, 102.0, 103.0, 104.0, 105.0, 106.0, 107.0, 108.0, 109.0, 110.0
		]
	  }
	}
