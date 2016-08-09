import 'package:dart_parser/parser.dart';

void main() {
  CompilationUnit unit = parseCompilationUnit(r'''
class A {
  foo() {
    print('a');
  }
}
get bar => 'ok';
''');

  print(unit.declarations.length);
  print(unit);
}
