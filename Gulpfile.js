var gulp = require('gulp'),
    plumber = require('gulp-plumber'),
    gutil = require('gulp-util'),
    uglify = require('gulp-uglify'),
    buffer = require('vinyl-buffer'),
    jshint = require('gulp-jshint'),
    react = require('gulp-react'),
    clean = require('gulp-clean'),
    compass = require('gulp-compass'),
    sourcemaps = require('gulp-sourcemaps'),
    bower = require('gulp-bower'),
    shell = require('gulp-shell'),
    browserify = require('gulp-browserify');

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
        .pipe(sourcemaps.init({loadMaps: process.env.NODE_ENV !== 'production'}))
        .pipe(buffer())
        .pipe(uglify())
        .pipe(sourcemaps.write('./'))
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
    return gulp.src('sass/main.scss')
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
    // start haskell server, there is probably
    // a cleaner way to do this
    gulp.src('*', {read: false})
        .pipe(shell('PORT=5000 cabal run'));

    gulp.watch(['client/**/*.js', 'sass/**/*.scss'], ['dev'])
        .on('error', handleError);

});

gulp.task('default', ['dev', 'watch']);
