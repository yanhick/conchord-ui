var gulp = require('gulp'),
    plumber = require('gulp-plumber'),
    gutil = require('gulp-util'),
    jshint = require('gulp-jshint'),
    react = require('gulp-react'),
    browserify = require('gulp-browserify');

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

    //prevent 'watch' crash in case of error
    var r = react({ es6module: true });
    r.on('error', function (e) {
        gutil.log(e);
        r.end();
    });

    gulp.src('client/**/*.js')
        .pipe(plumber())
        .pipe(r)
        .pipe(jshint())
        .pipe(jshint.reporter('default'));
});

gulp.task('browserify', function () {
    gulp.src('client/index.js')
        .pipe(plumber())
        .pipe(browserify({
            debug: !gulp.env.production,
            transform: ['babelify']
        }))
        .pipe(gulp.dest('dist/app.js'));
});

gulp.task('watch', ['lint'], function () {
    server.listen(serverPort);
    refresh.listen(livereloadPort);

    gulp.watch(['client/**/*.js'], ['lint', 'browserify'])
        .on('error', gutil.log);
});

gulp.task('default', ['dev', 'watch']);
