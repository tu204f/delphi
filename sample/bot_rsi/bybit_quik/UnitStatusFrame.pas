unit UnitStatusFrame;

interface

{$i platform.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Edit,
  Lb.SysUtils,
  Lb.Status;

type
  ///<summary>
  /// Статус информации
  ///</summary>
  ///<remarks>
  /// Здесь управления — осуществляется чтение
  ///</remarks>
  TStatusFrame = class(TFrame)
    Rectangle1: TRectangle;
    EditQty: TEdit;
    EditValueRSI: TEdit;
  private
    procedure QuikStatusOnUpDate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TStatusFrame }

constructor TStatusFrame.Create(AOwner: TComponent);
begin
  inherited;
  Status.OnUpDate := QuikStatusOnUpDate;
end;

destructor TStatusFrame.Destroy;
begin

  inherited;
end;

procedure TStatusFrame.QuikStatusOnUpDate(Sender: TObject);
begin
  // нужно для вывода информации
  EditValueRSI.Text :=
    'RSI:' + FloatToStr(Status.FastRSI) +
    '/' + FloatToStr(Status.SlowRSI) +
    ' Price: ' + FloatToStr(Status.Ask) +
    '/' + FloatToStr(Status.Bid);

  if Status.Position.Qty <> 0 then
  begin
    var xS :=
      '(' + GetStrToTypeSide(Status.Position.Side) + ')' +
      FloatToStr(Status.Position.Qty) + ' P:' +
      FloatToStr(Status.Position.Profit);
    EditQty.Text := xS;
  end
  else
    EditQty.Text := '';
end;

end.
