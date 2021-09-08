unit Lb.TreeViewFrame;

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
  FMX.TreeView;

type
  TTreeViewFrame = class(TFrame)
    TreeView: TTreeView;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TTreeViewFrame }

constructor TTreeViewFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TTreeViewFrame.Destroy;
begin

  inherited;
end;

end.
