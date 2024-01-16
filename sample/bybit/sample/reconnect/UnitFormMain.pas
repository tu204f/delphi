unit UnitFormMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  Lb.Bybit.Test,
  Lb.Bybit.SysUtils,
  Lb.Bybit.ServerTime;

type
  ///<summary>Контролируем перезапуск, состоние соединение бота</summary>
  TMainForm = class(TForm)
    Memo: TMemo;
    procedure FormShow(Sender: TObject);
  private
    BybitTest: TBybitTest;
    procedure BybitTestOnEventSing(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLog(S: String);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  BybitTest := TBybitTest.Create;
  BybitTest.OnEventSing := BybitTestOnEventSing;
  BybitTest.TimeOut := 50;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(BybitTest);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  BybitTest.Start;
end;

procedure TMainForm.SetLog(S: String);
var
  xS: String;
begin
  xS := FormatDateTime('hh:mm:ss.zzz',Time) + '>>' + S;
  Memo.Lines.Insert(0,xS);
end;

procedure TMainForm.BybitTestOnEventSing(Sender: TObject);
begin
  SetLog('MainForm.BybitTestOnEventSing');
end;

end.
