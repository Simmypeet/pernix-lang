//! Target invoking drop on the recursive data structure such as `ConsList` and
//! `dropAt` intrinsic

use crate::compile_file_with;

const SOURCE: &str = r#"
from core import Drop, Copy, dropAt, NoDrop, sizeof


extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32
    public function malloc(size: usize) -> *mut ()
    public function free(pointer: *mut ())
    public function memcpy(dst: *mut (), src: *(), size: usize) -> *mut ()


public struct Box[T]:
    private pointer: *mut T
    private _marker: phantom T


final implements[T] Copy[Box[T]] delete


implements[T] Drop[Box[T]]:
    function drop(self: &mut Box[T]):
        dropAt(self->pointer)
        free(self->pointer as *mut ())


implements[T] Box[T]:
    public function new(value: T) -> this:
        let newPointer = malloc(sizeof[T]())
        memcpy(newPointer, (&value) as *(), sizeof[T]())
        
        // don't invoke drop, it has been moved to the heap memory
        let .. = NoDrop { value: value }

        return Box {
            pointer: newPointer as *mut T,
            _marker: phantom
        }


public struct LoudDrop:
    private value: int32


final implements Drop[LoudDrop]:
    function drop(self: &mut LoudDrop):
        printf(&"Dropping %d\n\0"->[0], self->value)


final implements Copy[LoudDrop] delete


public struct Cons[T]:
    public value: T
    public list: Box[List[T]]


public enum List[T]:
    Nil
    Cons(Cons[T])


public function main():
    let mut first = 0i32
    let mut second = 0i32
    let mut third = 0i32

    scanf(&"%d %d %d\0"->[0], &mut first, &mut second, &mut third)

    let consList = List::Cons(Cons {
        value: LoudDrop { value: first },
        list: Box::new(List::Cons(Cons {
            value: LoudDrop { value: second },
            list: Box::new(List::Cons(Cons {
                value: LoudDrop { value: third },
                list: Box::new(List::Nil)
            }))
        }))
    })

"#;

#[test]
fn drop() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("12 24 36\n");
    });

    assert!(output.status.success());

    let stdout = String::from_utf8(output.stdout).unwrap();
    assert_eq!(stdout, "Dropping 12\nDropping 24\nDropping 36\n");
}
