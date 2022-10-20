unit UnitCandelsFrame;

interface

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
  FMX.Layouts,
  Lb.Candel.SysUtils,
  UnitCandelFrame;

type
  TCandelsFrame = class(TFrame)
    Layout: TLayout;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private
    FCandelFrames: TCandelFrameList;
  protected
    function GetCreateCandelFrame: TCandelFrame;
    procedure SetPositionCandelFrames;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetCandels(const ACandels: TCandelList);
  end;

implementation

{$R *.fmx}

{ TCandelsFrame }

constructor TCandelsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCandelFrames := TCandelFrameList.Create;
end;

destructor TCandelsFrame.Destroy;
begin
  FreeAndNil(FCandelFrames);
  inherited;
end;

function TCandelsFrame.GetCreateCandelFrame: TCandelFrame;
var
  xCandelFrame: TCandelFrame;
begin
  xCandelFrame := TCandelFrame.Create(nil);
  xCandelFrame.Parent := Layout;
  FCandelFrames.Add(xCandelFrame);
  Result := xCandelFrame;
end;

procedure TCandelsFrame.SetPositionCandelFrames;
var
  xCandelFrame: TCandelFrame;
  i, iCount: Integer;
var
  xX, xY, xWidth, xHeight, xDelta: Single;
begin
  iCount := FCandelFrames.Count;
  if iCount > 0 then
  begin
    xDelta := Self.Width/iCount;
    for i := 0 to iCount - 1 do
    begin
      xCandelFrame := FCandelFrames[i];
      // ---------------------------------------------
      xX      := xDelta * i;
      xY      := 0;
      xWidth  := xDelta;
      xHeight := Self.Height;
      // ---------------------------------------------
      xCandelFrame.SetBounds(xX, xY, xWidth, xHeight);
      xCandelFrame.SetShowCandel;
    end;
  end;
end;

procedure TCandelsFrame.TimerTimer(Sender: TObject);
begin
  SetPositionCandelFrames;
end;

procedure TCandelsFrame.SetCandels(const ACandels: TCandelList);

  procedure SetMaxAndMin(const ACandels: TCandelList; var AMaxValue, AMinValue: Double);
  var
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    AMaxValue := 0;
    AMinValue := 0;
    iCount := ACandels.Count;
    if iCount > 0 then
    begin
      xCandel := ACandels[0];
      AMaxValue := xCandel.High;
      AMinValue := xCandel.Low;
      for i := 1 to iCount - 1 do
      begin
        xCandel := ACandels[i];
        if xCandel.High > AMaxValue then AMaxValue := xCandel.High;
        if xCandel.Low  < AMinValue then AMinValue := xCandel.Low;
      end;
    end;
  end;

var
  xCandel: TCandel;
  i, iCount: Integer;
  xMaxValue, xMinValue: Double;
  xCandelFrame: TCandelFrame;
begin
  SetMaxAndMin(ACandels, xMaxValue, xMinValue);
  if xMaxValue > xMinValue then
  begin
    iCount := ACandels.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := ACandels[i];
        if FCandelFrames.Count <= i  then
          xCandelFrame := GetCreateCandelFrame
        else
          xCandelFrame := FCandelFrames[i];

        xCandelFrame.SetCandel(xMaxValue,xMinValue,xCandel);
      end;
  end
  else
    FCandelFrames.Clear;
end;

end.
