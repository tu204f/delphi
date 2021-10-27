unit Lb.SysUtils;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  TParamValue = record
    Number: Integer;
    Text: String;
  private
    function GetValue: String;
    procedure SetValue(const Value: String);
  public
    property Value: String read GetValue write SetValue;
  end;
  TParamValueList = TList<TParamValue>;



  ///<summary>Поток – который генерирует файл</summary>
  TBaseThread = class(TThread)
  private
    FOnBegin: TNotifyEvent;
    FOnEnd: TNotifyEvent;
  protected
    procedure DoBegin; virtual;
    procedure DoEnd; virtual;
    procedure DoWork; virtual;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property OnBegin: TNotifyEvent write FOnBegin;
    property OnEnd: TNotifyEvent write FOnEnd;
  end;

implementation

{ TParamValue }

function TParamValue.GetValue: String;
begin
  Result := IntToStr(Self.Number) + '.' + Self.Text;
end;

procedure TParamValue.SetValue(const Value: String);
var
  xNumber: String;
  xPosition: Integer;
begin
  xPosition := Pos('.',Value);
  xNumber := Copy(Value,1,xPosition - 1);
  Self.Number := StrToIntDef(xNumber,0);
  if Self.Number = 0 then
    raise Exception.Create('Error Message: Неверно указано число');
  Self.Text := Copy(Value,xPosition + 1,Value.Length - xPosition);
end;

{ TBaseThread }

constructor TBaseThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

destructor TBaseThread.Destroy;
begin

  inherited;
end;

procedure TBaseThread.DoBegin;
begin
  if Assigned(FOnBegin) then FOnBegin(Self);
end;

procedure TBaseThread.DoEnd;
begin
  if Assigned(FOnEnd) then FOnEnd(Self);
end;

procedure TBaseThread.DoWork;
begin

end;

procedure TBaseThread.Execute;
begin
  try
    Synchronize(DoBegin);
    Self.DoWork;
    Synchronize(DoEnd);
  except
    on E : Exception do
      raise Exception.Create('Error Message: ' + E.Message);
  end;
end;

end.
