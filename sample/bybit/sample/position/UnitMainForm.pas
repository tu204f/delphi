unit UnitMainForm;

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
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.StdCtrls,

  Lb.Bybit.SysUtils,
  Lb.Bybit.Position, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm)
    RectangleTop: TRectangle;
    ButtonStart: TButton;
    Memo: TMemo;
    procedure ButtonStartClick(Sender: TObject);
  private
    BybitPosition: TBybitPosition;
    procedure BybitPositionOnEventException(Sender: TObject);
    procedure BybitPositionOnEventEndLoading(Sender: TObject);
    procedure BybitPositionOnEventMessage(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}


constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  BybitPosition := TBybitPosition.Create;
  BybitPosition.Category := TTypeCategory.tcLinear;
  BybitPosition.Symbol := 'ETHUSDT';

  BybitPosition.OnEventException := BybitPositionOnEventException;
  BybitPosition.OnEventMessage := BybitPositionOnEventMessage;
  BybitPosition.OnEventEndLoading := BybitPositionOnEventEndLoading;

  BybitPosition.SetEncryption(
    't0YI4Ou0TKOTd7WrkE',
    'dWcdTGIulDoKOiK4mggPQIkYwmMFGxvFVusp'
  );
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(BybitPosition);
  inherited;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  BybitPosition.Start(1000);
end;

procedure TMainForm.BybitPositionOnEventEndLoading(Sender: TObject);
begin
  Memo.Lines.Add('BybitPositionOnEventEndLoading');
  Memo.Lines.Add('  количество: записей ' + BybitPosition.PositionObjects.Count.ToString);
end;


procedure TMainForm.BybitPositionOnEventMessage(Sender: TObject);
begin
  Memo.Lines.Clear;
  Memo.Lines.Add('>> 1. ' + BybitPosition.ValueMessage);
end;

procedure TMainForm.BybitPositionOnEventException(Sender: TObject);
begin
  Memo.Lines.Add('>> 2. ' + BybitPosition.ValueMessage);
end;

end.
