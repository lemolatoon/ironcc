use std::rc::Rc;

use ironcc::preprocess::Preprocessor;
use ironcc::preprocess::PreprocessorTokenStream;
use ironcc::preprocess::TokenKind;
use ironcc::tokenize::FileInfo;

#[test]
pub fn test_preprocess() {
    let input = "abc";
    let file_info = Rc::new(FileInfo::new("test.c".to_string(), input.to_string()));
    let preprocessed = Preprocessor::new("NO_INCLUDE_PATH").preprocess(file_info.clone());
    let mut stream = PreprocessorTokenStream::new(preprocessed.into_iter());
    let next = stream.next();
    assert!(matches!(next, Some((_, 'a'))), "{:?}", next);
    let next = stream.next();
    assert!(matches!(next, Some((_, 'b'))), "{:?}", next);
    let next = stream.next();
    assert!(matches!(next, Some((_, 'c'))), "{:?}", next);
    let next = stream.next();
    assert!(matches!(next, None), "{:?}", next);
}
