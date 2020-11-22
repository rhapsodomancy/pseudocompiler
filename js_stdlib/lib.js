/**
 * Makeshift standard library/runtime for the JS backend to the compiler.
 */

let log = [];

/**
 * The print function.
 */
function print(x) {
    log.push(x);
}
