var gulp = require('gulp'),
    gutil = require('gulp-util'),
    jshint = require('gulp-jshint'),
    react = require('gulp-react'),
    browserify = require('browserify'),
    babelify = require('babelify'),
    concat = require('gulp-concat');
    fs = require('fs');

var express = require('express'),
    refresh = require('gulp-livereload'),
    livereload = require('connect-livereload');

var livereloadPort = 35729,
      serverPort = 5000;

var server = express();

server.use(livereload({port: livereloadPort}));

server.all('/*', function (req, res) {
    res.sendfile('index.html', {root: 'dist'});
});

// Dev task
gulp.task('dev', ['lint', 'browserify'], function () {});

// Lint task
gulp.task('lint', function () {
    gulp.src('client/**/*.js')
        .pipe(react({ es6module: true }))
        .on('error', gutil.log)
        .pipe(jshint())
        .pipe(jshint.reporter('default'));
});

gulp.task('browserify', function () {
    browserify({ debug: true })
        .transform(babelify)
        .require('./client/index.js', { entry: true })
        .bundle()
        .pipe(fs.createWriteStream('dist/app.js'));
});

gulp.task('watch', ['lint'], function () {
    server.listen(serverPort);
    refresh.listen(livereloadPort);

    gulp.watch(['client/**/*.js'], [
        'lint',
        'browserify'
    ]);
});

gulp.task('default', ['dev', 'watch']);
