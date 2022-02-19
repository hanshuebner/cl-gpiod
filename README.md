# cl-gpiod - A Common Lisp library to interact with Linux GPIO ports

This library interfaces to
[libgpiod](https://git.kernel.org/pub/scm/libs/libgpiod/libgpiod.git/)
which provides access to GPIO ports.

## libgpiod CFFI definitions

CFFI definitions for all of libgpiod's functionality are provided,
except for the deprecated functions.  All functions and macros are
provided in Common Lisp syntax, i.e. underscores are replaced by minus
signs, upper case macro definitions are turned to lower case constants
with leading and trailing plus signs, the `gpio_` prefix is removed to
be replaced by the `gpiod:` package alias.   Thus,
`gpiod_chip_open_by_name` becomes `gpio:chip-open-by-name`.

## cl-gpiod high-level interface

A `define-gpio` macro is provided, which is used to define all the
bits that an application wants to use on a single GPIO chip.  It
introduces the concept of "ports", which can either be single or
groups of arbitray bits.  For each port, getter and setter functions
are defined.

Single-bit ports are accessed as booleans when reading or writing.
The logic for input bits can be reversed by setting the `:active-low`
flag.  Multi-bit ports read and write as integer values.

Here is a short example of the high-level interface:

```
(cl-gpiod:define-gpio demo-board
  :chip-name "gpiochip0"
  :ports ((data-out :lines (23 24 25)
                    :direction :output)
          (button-1 :line 17
                    :direction :input
                    :flags (:bias-pull-up :active-low))
          (button-2 :line 18
                    :direction :input
                    :flags (:bias-pull-up :active-low))))
```

Here, bits 23, 24 and 25 are grouped as multi-bit output port named
`data-out`.  It can be written like so:

    (setf (data-out) 3)

Bits 17 and 18 are defined as input ports.  They can be read like so:

    (when (button-1) ...)

Before the accessors can be used, the GPIO chip must be opened using
`cl-gpiod:open-chip`.  A "consumer name" needs to be provided which
will be shown in `gpioinfo` to help identify users of GPIO bits:

    (cl-gpiod:open-chip demo-board "demo")

```
apple2-dev 197_% gpioinfo                                                                                                               ±[●●][main]
gpiochip0 - 54 lines:
    line   0:      unnamed       unused   input  active-high
[...]
    line  17:      unnamed       "demo"   input   active-low [used pull-up]
    line  18:      unnamed       "demo"   input   active-low [used pull-up]
    line  19:      unnamed       unused   input  active-high
    line  20:      unnamed       unused   input  active-high
    line  21:      unnamed       unused   input  active-high
    line  22:      unnamed       unused  output  active-high
    line  23:      unnamed       "demo"  output  active-high [used]
    line  24:      unnamed       "demo"  output  active-high [used]
    line  25:      unnamed       "demo"  output  active-high [used]
    line  26:      unnamed       unused   input  active-high
```


# TODO

 - Verify bit order
 - Event interface
 - Better documentation
 - Better open/close experience
