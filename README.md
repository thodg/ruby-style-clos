Ruby style classes for the Common Lisp Object System
====================================================

One of the strongest chapter of Common Lisp is its powerful object system,
simply named CLOS. Hopefully objects enable a very convenient way of
representing unique and inherited behaviours, shared between all instances
of the class.

However before mastering the whole art of the Common Lisp meta-object
protocol we have to get ourselves into some arcane concepts which are not
directly related to how programmers are used to work with objects and
classes. Most programming languages provide only single dispatch
polymorphism, and more modern languages like Ruby and Javascript do not
require declaration of all attributes before using them.

Of course, for the more advanced Lisp programmers this is not a problem.
Syntax is just a macro away and hacking CLOS to provide a lean interface
to most usages of classes is just a sunny vacation trip on a beautiful,
deserted sea shore.

Now that we are introduced, it strikes the experienced coder that the Ruby
way of defining classes is a pleasure to work, with minimal introducion of
syntax for instance and class variables prefixed with one or two `@`
respectively. More embarassing is the introduction of the `self` reserved
symbol that is used to represent the current object inside its class methods.

As an example is worth a thousand explanations, it might look really
unconventional for seasoned lispers but a few crazies might find it
refreshing to the eye when working with many complex classes.

```lisp
(use-package #:ruby-style-clos)

(class animal)

(class dog (animal)

  (def initialize (&optional barked)
    (setf @barked barked))

  (def foo ()
    @foo)

  (def bark ()
    (if @barked
	(print "Pant !")
	(print "Woof !"))
    (setf @barked t)
    self))

(bark (bark (new 'dog)))
```

This would essentially expand to the following code :

```lisp
(defclass animal () ())

(progn

  (defclass dog (animal)
    ((barked :initform nil)
     (foo :initform nil)))

  (defmethod initialize ((self dog) &rest arguments)
    (destructuring-bind (&optional barked) arguments
      (setf (slot-value self 'barked) barked)))

  (defmethod foo ((self dog) &rest arguments)
    (destructuring-bind () arguments
      (slot-value self 'foo)))

  (defmethod bark ((self dog) &rest arguments)
    (destructuring-bind () arguments
      (if (slot-value self 'barked)
	    (print "Pant !")
	    (print "Woof !"))
      (setf (slot-value self 'barked) t)
      self))

  (defmethod print-object ((object dog) stream)
    (print-unreadable-object (object stream :identity t)
      (write-string (string-capitalize (symbol-name 'dog)) stream)
      (write-char #\  stream)
      (write-char #\@ stream)
      (write-string (string-downcase (symbol-name 'barked)) stream)
      (write-char #\= stream)
      (prin1 (slot-value object 'barked) stream)
      (write-char #\  stream)
      (write-char #\@ stream)
      (write-string (string-downcase (symbol-name 'foo)) stream)
      (write-char #\= stream)
      (prin1 (slot-value object 'foo) stream)))

  (find-class 'dog))
```

As you can see, while the former syntax is much shorter, the code produced
by the `CLASS` macro looks like traditional CLOS style code and in bonus
we get to see the slot values in the printed object representation.

The intent was to provide a simple way to define simple classes which we
encounter more often, and reduce boilerplate code and improve readability
while introducing as little syntax as possible. Hopefully the Ruby style
syntax is an effective way of achieving that noble goal, saving our poor
eyes from the evil repeatedness of complex code.

In an effort to provide learning material for Lisp teachers, this could
do for a nice introduction to both objects in Lisp and macros, by introducing
a familiar syntax to programmers coming from other languages.
