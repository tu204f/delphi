unit Lb.Json;

interface

uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections;

type
  TJsObject = class;
  TJsObjectList = TObjectList<TJsObject>;

  ///<summary>
  /// Объект Json
  ///</summary>
  TJsObject = class(TObject)
  private

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  ///<summary>
  /// Список объектов
  ///</summary>
  TJson = class(TObject)
  private
    FJsObjects: TJsObjectList;
    function GetText: String;
    procedure SetText(const Value: String);
  protected
    procedure SetParser(const ASource: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Text: String read GetText write SetText;
  end;


implementation

{ TJsObject }

constructor TJsObject.Create;
begin

end;

destructor TJsObject.Destroy;
begin

  inherited;
end;

{ TJson }

constructor TJson.Create;
begin

end;

destructor TJson.Destroy;
begin

  inherited;
end;

procedure TJson.SetParser(const ASource: String);

  function _IsChr(const C: Char): Boolean;
  begin
    Result := CharInSet(C,[#32,#10,#9,#13]);
  end;

var
  xC: Char;
begin
  if not ASource.IsEmpty then
    raise Exception.Create('Error Message: А что парсить все пусто');
  for xC in ASource do
  begin
    if _IsChr(xC) then
      Continue;


  end;
end;

function TJson.GetText: String;
begin

end;

procedure TJson.SetText(const Value: String);
begin

end;

end.
