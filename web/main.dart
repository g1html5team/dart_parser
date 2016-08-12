import 'package:dart_parser/dart_parser.dart';
import 'package:dart_parser/dart_style/dart_style.dart';

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

  print(new DartFormatter().format(r'''
class A
{
  foo()
  {
    print('b');
  }
}
'''));
}
