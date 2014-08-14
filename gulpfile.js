var gulp = require('gulp')
  , clean = require('gulp-clean')
  , gutil = require('gulp-util')
  , plumber = require('gulp-plumber')
  , purescript = require('gulp-purescript')
  , http = require('http')
  , nstatic = require('node-static')
  , config = {
      clean: ['dist', 'js', 'externs'],
      purescript: {
        src: [
          'bower_components/purescript-*/src/**/*.purs*',
          'src/**/*.purs'
        ],
        examples: {
          todomvc: 'examples/Todomvc/**/*.purs'
        },
        dest: 'dist',
        docgen: 'MODULE.md',
        options: {
          main: 'Todomvc.Main',
          output: 'todomvc.js'
        }
      },
      nstatic: {
        root: '.',
        port: 9501
      }
    }
;

function error(e) {
  gutil.log(gutil.colors.magenta('>>>> Error <<<<') + '\n' + e.toString().trim());
  this.emit('end');
}

function server(cb) {
  var file = new nstatic.Server(config.nstatic.root);
  http.createServer(function(request, response){
    request.addListener('end', function(){
      file.serve(request, response);
    }).resume();
  }).listen(config.nstatic.port, function(){
    gutil.log('Listening on port ' + gutil.colors.magenta(config.nstatic.port));
    cb();
  });
}

gulp.task('clean', function(){
  return (
    gulp.src(config.clean, {read: false}).
      pipe(clean())
  );
});

gulp.task('todomvc', ['clean'], function(){
  return (
    gulp.src([config.purescript.examples.todomvc].concat(config.purescript.src)).
    pipe(plumber()).
    pipe(purescript.psc(config.purescript.options)).
    on('error', error).
    pipe(gulp.dest(config.purescript.dest))
  );
});

gulp.task('make', function(){
  return (
    gulp.src(config.purescript.src).
    pipe(plumber()).
    pipe(purescript.pscMake({output: config.purescript.dest})).
    on('error', error)
  );
});

gulp.task('docgen', function(){
  return (
    gulp.src(config.purescript.src[1]).
    pipe(plumber()).
    pipe(purescript.docgen()).
    on('error', error).
    pipe(gulp.dest(config.purescript.docgen))
  );
});

gulp.task('watch', function(cb){
  gulp.watch(config.purescript.src, ['make']);
});

gulp.task('watch.todomvc', function(cb){
  gulp.watch([config.purescript.examples.todomvc].concat(config.purescript.src), ['todomvc']);
  server(cb);
});

gulp.task('default', ['clean', 'make', 'docgen']);
