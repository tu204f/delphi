unit UnitLevelsFrame;

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
  Lb.Level,
  FMX.Layouts,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid;

type
  TLevelsFrame = class(TFrame)
    LayoutGrid: TLayout;
  private
    Levels: TOneEventLevelList;
    procedure EventOnIntersectionLevel(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetInitilizationLevel;

    procedure Clear;
    procedure AddLevel(const ALevel: TOneEventLevel);
    procedure DeletedLevel(const ALevel: TOneEventLevel);

    procedure SetUpData(const AValueRSI: Double);
  end;

implementation

{$R *.fmx}

{ TLevelsFrame }

constructor TLevelsFrame.Create(AOwner: TComponent);
begin
  inherited;
  Levels := TOneEventLevelList.Create;

  SetInitilizationLevel;
end;


destructor TLevelsFrame.Destroy;
begin
  FreeAndNil(Levels);
  inherited;
end;

procedure TLevelsFrame.SetInitilizationLevel;
begin

end;

procedure TLevelsFrame.EventOnIntersectionLevel(Sender: TObject);
begin
  // Событие от уровня

end;

procedure TLevelsFrame.SetUpData(const AValueRSI: Double);
begin

end;

procedure TLevelsFrame.Clear;
begin

end;

procedure TLevelsFrame.AddLevel(const ALevel: TOneEventLevel);
begin

end;

procedure TLevelsFrame.DeletedLevel(const ALevel: TOneEventLevel);
begin

end;



end.
