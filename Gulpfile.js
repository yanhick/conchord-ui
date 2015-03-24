var gulp = require('gulp'),
    plumber = require('gulp-plumber'),
    gutil = require('gulp-util'),
    jshint = require('gulp-jshint'),
    react = require('gulp-react'),
    clean = require('gulp-clean'),
    compass = require('gulp-compass'),
    bower = require('gulp-bower'),
    browserify = require('gulp-browserify');

var server = require('./server'),
    refresh = require('gulp-livereload'),
    livereload = require('connect-livereload');

var livereloadPort = 35729,
      serverPort = 5000;

server.use(livereload({port: livereloadPort}));

// handle all tasks errors
function handleError(e) {

    gutil.log(e);

    //if we are in a testing or production environement
    //(for example CI server or deploy procedure),
    //we want to crash the process instead of ignoring the error
    if (process.env.NODE_ENV === 'test' || process.env.NODE_ENV === 'production') {
        throw e;
    }
}

// Dev task
gulp.task('dev', ['clean', 'compass', 'lint', 'static', 'browserify'], function () {});

// Clean output task
gulp.task('clean', function () {
    return gulp.src('dist', { read: false })
        .pipe(plumber({ errorHandler: handleError }))
        .pipe(clean());
});

// Lint task
gulp.task('lint', function () {

    //prevent 'watch' crash in case of error
    var r = react({ es6module: true });
    r.on('error', function (e) {
        handleError(e);
        r.end();
    });

    return gulp.src('client/**/*.js')
        .pipe(plumber({ errorHandler: handleError }))
        .pipe(r)
        .pipe(jshint())
        .pipe(jshint.reporter('default'));
});

// Copy static files to output folder
gulp.task('static', ['clean'], function () {
    return gulp.src('static/**/*.*')
        .pipe(plumber({ errorHandler: handleError }))
        .pipe(gulp.dest('dist'));
});

gulp.task('browserify', ['clean'], function () {
    return gulp.src('client/index.js')
        .pipe(plumber({ errorHandler: handleError }))
        .pipe(browserify({
            debug: process.env.NODE_ENV !== 'production',
            transform: ['babelify']
        }))
        .pipe(gulp.dest('dist/js'));
});

gulp.task('bower', ['clean'], function () {
    return bower()
        .pipe(gulp.dest('./bower_components/'));
});

gulp.task('icons', ['clean'], function () {
    return gulp.src('./bower_components/fontawesome/fonts/**.*')
        .pipe(gulp.dest('dist/fonts'));
});

gulp.task('compass', ['clean', 'bower', 'icons'], function () {
    return gulp.src('sass/style.scss')
        .pipe(plumber({ errorHandler: handleError }))
        .pipe(compass({
            style: 'compressed',
            import_path: [
                'sass',
                './bower_components/bootstrap-sass-official/assets/stylesheets',
                './bower_components/fontawesome/scss',
            ]
        }))
        .pipe(gulp.dest('dist/css'));
});

gulp.task('watch', ['lint'], function () {
    server.listen(serverPort);
    refresh.listen(livereloadPort);

    gulp.watch(['client/**/*.js'], ['lint', 'browserify'])
        .on('error', handleError);

    gulp.watch('./sass/**/*.scss', ['compass'])
        .on('error', handleError);

    gulp.watch('./dist/**')
        .on('error', handleError)
        .on('change', refresh.changed);
});

gulp.task('default', ['dev', 'watch']);
