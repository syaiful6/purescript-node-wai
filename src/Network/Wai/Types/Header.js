
// module Network.HTTP.Types.Header

exports.readIntegerImpl = function (nothing, just, tuple, string) {
  var negate = false, str = string;
  if (str === "") return nothing;
  if (str[0] === '-') {
    str = str.substring(1);
    negate = true;
  } else if (str[0] === '+') {
    str = str.substring(1);
  }
  var matched = str.match(/\d+/);
  if (matched == null && matched.length === 0) {
    return nothing;
  } else {
    var ix = matched[0],
        idx = matched.index + ix.length,
        leftover = str.substring(idx),
        pix = parseInt(ix);
    return just(tuple(negate ? -pix : pix)(leftover));
  }
}
