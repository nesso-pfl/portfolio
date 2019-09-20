var firebase = require("firebase");

exports.sdkVersion_ = function() {
  return firebase.SDK_VERSION;
};

exports.initializeApp_ = function(options, name) {
  return firebase.initializeApp(options, name);
};

exports.firestore_ = function(app) {
  return app.firestore();
};

exports.collection_ = function(collectionPath, firestore) {
  return firestore.collection(collectionPath);
};

exports.add_ = function(docData, colRef) {
  return colRef.add(docData);
};

exports.doc_ = function(documentPath, firestore) {
  return firestore.doc(documentPath);
};

exports.get_ = function(options, firestore) {
  return firestore.get(options);
};

exports.data_ = function(snapShot) {
  return snapShot.data();
};

exports.update_ = function(data, docRef) {
  return docRef.update(data);
};
