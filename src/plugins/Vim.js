var kuromoji = require("kuromoji");

exports.tokenize_ = function(s){
  return kuromoji.builder().build(function (err, tokenizer) {
    return tokenizer.tokenize(s);
  });
};
