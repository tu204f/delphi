unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  Lb.SysUtils;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    ButtonLoad: TButton;
    Timer: TTimer;
    ButtonStart: TButton;
    procedure ButtonLoadClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
  private
    { Private declarations }
  public
    Candels: TCandelList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure SetToCandels(ASources: TStrings; ACandels: TCandelList);

  procedure _SetParser(const S: String; ASourceParser: TStrings);
  var
    xC: Char;
    tmpS: String;
  begin
    tmpS := '';
    for xC in S do
    begin
      if xC = ';' then
      begin
        if not tmpS.IsEmpty then
          ASourceParser.Add(tmpS);
        tmpS := '';
      end
      else
        tmpS := tmpS + xC;
    end;
    if not tmpS.IsEmpty then
      ASourceParser.Add(tmpS);
  end;

  function _GetCandel(const S: String): TCandel;
  var
    xCandel: TCandel;
    xSourceParser: TStrings;
  begin
    FillChar(xCandel,SizeOf(xCandel),0);
    xSourceParser := TStringList.Create;
    try
      _SetParser(S,xSourceParser);
      if xSourceParser.Count >= 7 then
      begin
        xCandel.Time  := StrToInt64Def(xSourceParser[1],0);
        xCandel.Open  := StrToFloatDef(xSourceParser[2],0);
        xCandel.High  := StrToFloatDef(xSourceParser[3],0);
        xCandel.Low   := StrToFloatDef(xSourceParser[4],0);
        xCandel.Close := StrToFloatDef(xSourceParser[5],0);
        xCandel.Vol   := StrToFloatDef(xSourceParser[6],0);
      end;
    finally
      FreeAndNil(xSourceParser);
    end;
    Result := xCandel;
  end;

var
  xCandel: TCandel;
begin
  ACandels.Clear;
  for var xS in ASources do
  begin
    xCandel := _GetCandel(xS);
    ACandels.Add(xCandel);
  end;
end;


constructor TForm4.Create(AOwner: TComponent);
begin
  inherited;
  Candels := TCandelList.Create;
end;

destructor TForm4.Destroy;
begin
  FreeAndNil(Candels);
  inherited;
end;

procedure TForm4.ButtonLoadClick(Sender: TObject);
var
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    xStr.LoadFromFile('history.csv');
    SetToCandels(xStr,Candels);
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TForm4.ButtonStartClick(Sender: TObject);
begin
  // Старт и остановка запуска оптимизации
  Timer.Enabled := not Timer.Enabled;
end;

procedure TForm4.TimerTimer(Sender: TObject);
begin
  // Управление популяций
end;


end.
