var firebase = require("firebase");

exports.apps_ = function() {
  return firebase.apps;
};

exports.app_ = function() {
  return firebase.app();
};

exports.initialized_ = function() {
  return Boolean(firebase.apps.length);
};

exports.sdkVersion_ = function() {
  return firebase.SDK_VERSION;
};

exports.initializeApp_ = function(options, name) {
  if (firebase.apps.length) {
    return firebase.app();
  } else {
    return firebase.initializeApp(options, name);
  }
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

exports.id_ = function(snapshot) {
  return snapshot.id;
};

exports.doc_ = function(documentPath, firestore) {
  return firestore.doc(documentPath);
};

exports.limit_ = function(n, query) {
  return query.limit(n);
};

exports.get_ = function(options, ref) {
  return ref.get();
};

exports.docs_ = function(querySnapshot) {
  return querySnapshot.docs;
};

exports.data_ = function(snapshot) {
  return snapshot.data();
};

exports.update_ = function(data, docRef) {
  return docRef.update(data);
};

exports.now_ = new firebase.firestore.Timestamp.now();

exports.seconds_ = function(timestamp) {
  return timestamp.seconds;
};

exports.toDate_ = function(timestamp) {
  return timestamp.toDate();
};

exports.auth_ = function(app) {
  return app.auth();
};

exports.currentUser_ = function(auth) {
  return auth.currentUser;
};

exports.signInUserWithEmailAndPassword_ = function(auth, email, password) {
  return auth.signInUserWithEmailAndPassword(email, password);
};
