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

exports.id_ = function(colRef) {
  return colRef.id;
};

exports.doc_ = function(documentPath, firestore) {
  return firestore.doc(documentPath);
};

exports.limit_ = function(n, query) {
  return query.limit(n);
};

exports.get_ = function(options, ref) {
  return function() {
    return ref.get();
  }
};

exports.docs_ = function(querySnapshot) {
  console.log(querySnapshot);
  return querySnapshot.docs;
};

exports.data_ = function(snapShot) {
  return snapShot.data();
};

exports.update_ = function(data, docRef) {
  return docRef.update(data);
};
