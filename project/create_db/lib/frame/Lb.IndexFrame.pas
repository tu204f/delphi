unit Lb.IndexFrame;

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
  Lb.Create.DB, FMX.Layouts;

type
  TIndexFrame = class(TFrame)
    Layout: TLayout;
    LayoutIndex: TLayout;
    GridPanelLayoutField: TGridPanelLayout;
  private
    FIndexTable: TCrIndex;
    FTable: TCrTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetParams(const ATable: TCrTable; const AIndexTable: TCrIndex);
    property IndexTable: TCrIndex read FIndexTable;
    property Table: TCrTable read FTable;
  end;

implementation

{$R *.fmx}

{ TIndexFrame }

constructor TIndexFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TIndexFrame.Destroy;
begin

  inherited;
end;

procedure TIndexFrame.SetParams(const ATable: TCrTable; const AIndexTable: TCrIndex);
begin
  FIndexTable := AIndexTable;
  FTable := ATable;
end;

end.
