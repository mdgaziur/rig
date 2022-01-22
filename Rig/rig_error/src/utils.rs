pub fn number_len(num: usize) -> usize {
    (0..).take_while(|i| 10usize.pow(*i) <= num).count()
}
