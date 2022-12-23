unit Script.Token;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Contnrs,
  System.Classes,
  System.Generics.Collections,
  System.RegularExpressions;

type

  TTypeToken = (
    ttBracket, // Скобка
    ttOperator,// Оператор
    ttEnd      // Конец
  );

  TTokens = class(TObject)
  public type
    TValues = array of String;
  private
    FValues: TValues;
  public
    procedure SetTokens(const AValue: TValues);
    procedure SetInitialization;
  end;

implementation

var
  localTokens: TTokens = nil;

{ TTokens }

procedure TTokens.SetInitialization;
begin
  Self.SetTokens([
    '+',     // Сложение
    '-',     // Вычитание
    '*',     // Умножение
    '/',     // Деление

    '(',')', // Открытие закрытие скобки

    ':=',    // Присвоение
    ';'      // конец строки выражение

    ]);
end;

procedure TTokens.SetTokens(const AValue: TValues);
begin
  FValues := AValue;
end;

initialization
  localTokens := TTokens.Create;

finalization
  FreeAndNil(localTokens);

end.
