use std::rc::Rc;

use ironcc::preprocess::preprocess::Preprocessor;
use ironcc::preprocess::preprocessor_streams::PreprocessorTokenStream;
use ironcc::preprocess::srccursor::SrcCursor;
use ironcc::tokenize::tokenize::FileInfo;

#[test]
pub fn test_preprocess() {
    let input = "abc";
    let file_info = Rc::new(FileInfo::new("test.c".to_string(), input.to_string()));
    let preprocessed = Preprocessor::new(file_info.clone(), "NO_INCLUDE_PATH")
        .preprocess(&mut SrcCursor::new(file_info), None)
        .unwrap();
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
