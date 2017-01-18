unit MyLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, DbCtrls, Grids, ComCtrls, Contnrs, UMBAuthor,
  UMBComposition, UMBEditor, UMBGenre, UMBPublisher, UMBTranslator, UMBBook;

type

  { TMyBooks }

  TMyBooks = class(TForm)
    btnBooks: TButton;
    btnAuthors: TButton;
    btnGenres: TButton;
    dtnAddBook: TButton;
    DataSource1: TDataSource;
    edtBookName: TEdit;
    edtBookISBN: TEdit;
    edtOrigName: TEdit;
    edtBookYear: TEdit;
    edtBookAnnotation: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure btnAuthorsClick(Sender: TObject);
    procedure btnGenresClick(Sender: TObject);
    procedure btnBooksClick(Sender: TObject);
    procedure dtnAddBookClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DeleteRowsFromStringGrid();
    procedure ReadBooksFromDB();
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MyBooks: TMyBooks;
  BooksList : TObjectList;

implementation

{$R *.lfm}

{ TMyBooks }

procedure TMyBooks.ReadBooksFromDB();
begin

     SQLQuery1.Close;
     SQLQuery1.SQL.Text:='SELECT id FROM books';
     SQLQuery1.Open;
     SQLQuery1.DisableControls;

     //ShowMessage(IntToStr(SQLQuery1.RecordCount));
     //SQLQuery1.First;
     while (not SQLQuery1.EOF) do
     begin
        ShowMessage(SQLQuery1.FieldByName('id').AsString);
        BooksList.Add(TMBBook.Create(SQLQuery1.FieldByName('id').AsInteger, SQLQuery1, SQLTransaction1));
        SQLQuery1.Next;
     end;

     SQLQuery1.EnableControls;
     SQLQuery1.Close;

end;

procedure TMyBooks.FormCreate(Sender: TObject);
var
  BookCount : Integer;
begin
  //ДК, получаем количество книг, создаем массив и саписываем туда значения
  SQLQuery1.Close;
  BooksList:=TObjectList.Create;
  ReadBooksFromDB();
end;

//ДК, очищаем таблицу, оставляем только строку с заголовками
procedure TMyBooks.DeleteRowsFromStringGrid();
var
  I : Integer;
begin
     StringGrid1.Clean([gzNormal]);

     if StringGrid1.RowCount > 1 then
     begin
         for I:=StringGrid1.RowCount downto 1 do;
              StringGrid1.DeleteRow(I);
     end;
     StringGrid1.RowCount:=1;

end;

procedure TMyBooks.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     SQLQuery1.Close;
     SQLTransaction1.Active:=False;
     SQLite3Connection1.Connected:=False;
end;

procedure TMyBooks.btnBooksClick(Sender: TObject);
var
  I : Integer;
  book : TMBBook;
begin
     DeleteRowsFromStringGrid();

     //ДК, обработка запроса, пытаемся вывести результат

     for I:=0 to BooksList.Count-1 do
     begin
          book:=BooksList.Items[I] as TMBBook;
          StringGrid1.InsertRowWithValues(I+1, [IntToStr(book.BookID), book.BookName, book.BookISBN, book.BookName, book.BookYear, book.BookNote]);
     end;
end;

procedure TMyBooks.dtnAddBookClick(Sender: TObject);
begin
     SQLQuery1.Close;
     SQLQuery1.SQL.Text:='INSERT INTO books (name, isbn, orig_name, year, note) VALUES (:bNAME, :bISBN, :bORIGNAME, :bYEAR, :bNOTE)';
     SQLQuery1.Params.ParamByName('bNAME').AsString:=edtBookName.Text;
     SQLQuery1.Params.ParamByName('bISBN').AsString:=edtBookISBN.Text;
     SQLQuery1.Params.ParamByName('bORIGNAME').AsString:=edtOrigName.Text;
     SQLQuery1.Params.ParamByName('bYEAR').AsString:=edtBookYear.Text;
     SQLQuery1.Params.ParamByName('bNOTE').AsString:=edtBookAnnotation.Text;
     SQLQuery1.ExecSQL;
     SQLTransaction1.Commit;
end;

procedure TMyBooks.btnGenresClick(Sender: TObject);
begin

end;

procedure TMyBooks.btnAuthorsClick(Sender: TObject);
var
  I : Integer;
  c: TGridColumn;
  ColumNNames : array[0..1] of string = ('Фамилия', 'Имя');
begin
      StringGrid1.Clean([gzNormal]);

     for I:= 0 to Length(ColumnNames)-1 do
     begin
          c := StringGrid1.Columns.Add;
          c.Title.Caption:=ColumnNames[I];
     end;
     //ДК, обработка запроса, пытаемся вывести результат
     I:=1;
     SQLQuery1.Close;
     SQLQuery1.SQL.Text:='SELECT surname, name FROM authors';
     SQLQuery1.Open;
     while not SQLQuery1.EOF do
     begin

        StringGrid1.RowCount:=StringGrid1.RowCount + 1;
        StringGrid1.InsertRowWithValues(I, [IntToStr(I), SQLQuery1.FieldByName('surname').AsString,
                                        SQLQuery1.FieldByName('name').AsString]);
        I:=I+1;
        SQLQuery1.Next;
     end;
     SQLQuery1.Close;
end;

end.

