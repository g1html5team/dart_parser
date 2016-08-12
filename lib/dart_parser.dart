// Copyright (c) 2013, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'package:dart_parser/dart/ast/ast.dart';
import 'package:dart_parser/src/dart/scanner/reader.dart';
import 'package:dart_parser/src/dart/scanner/scanner.dart';
import 'package:dart_parser/src/error.dart';
import 'package:dart_parser/src/generated/error.dart';
import 'package:dart_parser/src/generated/parser.dart';
import 'package:dart_parser/src/string_source.dart';
import 'package:dart_parser/src/generated/source.dart';

export 'package:dart_parser/dart/ast/ast.dart';
export 'package:dart_parser/dart/ast/visitor.dart';
export 'package:dart_parser/src/dart/ast/utilities.dart';
export 'package:dart_parser/src/error.dart';
export 'package:dart_parser/src/generated/error.dart';
export 'package:dart_parser/src/generated/utilities_dart.dart';

/// Parses a string of Dart code into an AST.
///
/// If [name] is passed, it's used in error messages as the name of the code
/// being parsed.
///
/// Throws an [AnalyzerErrorGroup] if any errors occurred, unless
/// [suppressErrors] is `true`, in which case any errors are discarded.
///
/// If [parseFunctionBodies] is [false] then only function signatures will be
/// parsed.
CompilationUnit parseCompilationUnit(String contents,
    {String name, bool suppressErrors: false, bool parseFunctionBodies: true}) {
  Source source = new StringSource(contents, name);
  return _parseSource(contents, source,
      suppressErrors: suppressErrors, parseFunctionBodies: parseFunctionBodies);
}

/// Parses the script tag and directives in a string of Dart code into an AST.
///
/// Stops parsing when the first non-directive is encountered. The rest of the
/// string will not be parsed.
///
/// If [name] is passed, it's used in error messages as the name of the code
/// being parsed.
///
/// Throws an [AnalyzerErrorGroup] if any errors occurred, unless
/// [suppressErrors] is `true`, in which case any errors are discarded.
CompilationUnit parseDirectives(String contents,
    {String name, bool suppressErrors: false}) {
  var source = new StringSource(contents, name);
  var errorCollector = new _ErrorCollector();
  var reader = new CharSequenceReader(contents);
  var scanner = new Scanner(source, reader, errorCollector);
  var token = scanner.tokenize();
  var parser = new Parser(source, errorCollector);
  var unit = parser.parseDirectives(token);
  unit.lineInfo = new LineInfo(scanner.lineStarts);

  if (errorCollector.hasErrors && !suppressErrors) throw errorCollector.group;

  return unit;
}

/// Converts an AST node representing a string literal into a [String].
String stringLiteralToString(StringLiteral literal) {
  return literal.stringValue;
}

CompilationUnit _parseSource(String contents, Source source,
    {bool suppressErrors: false, bool parseFunctionBodies: true}) {
  var reader = new CharSequenceReader(contents);
  var errorCollector = new _ErrorCollector();
  var scanner = new Scanner(source, reader, errorCollector);
  var token = scanner.tokenize();
  var parser = new Parser(source, errorCollector)
    ..parseFunctionBodies = parseFunctionBodies;
  var unit = parser.parseCompilationUnit(token)
    ..lineInfo = new LineInfo(scanner.lineStarts);

  if (errorCollector.hasErrors && !suppressErrors) throw errorCollector.group;

  return unit;
}

/// A simple error listener that collects errors into an [AnalyzerErrorGroup].
class _ErrorCollector extends AnalysisErrorListener {
  final _errors = <AnalysisError>[];

  _ErrorCollector();

  /// The group of errors collected.
  AnalyzerErrorGroup get group =>
      new AnalyzerErrorGroup.fromAnalysisErrors(_errors);

  /// Whether any errors where collected.
  bool get hasErrors => !_errors.isEmpty;

  @override
  void onError(AnalysisError error) => _errors.add(error);
}

