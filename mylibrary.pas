unit MyLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, DbCtrls, Grids, ComCtrls, ValEdit,
  Contnrs, UMBAuthor, UMBComposition, UMBEditor, UMBGenre, UMBPublisher,
  UMBTranslator, UMBBook;


type
    DataType =( Books, Compositions, Authors, Editors, Translators, Genres, Publishers);

  { TMyBooks }

  TMyBooks = class(TForm)
    btnAdd: TButton;
    DataSource1: TDataSource;
    PageControl1: TPageControl;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    BooksStringGrid: TStringGrid;
    AuthorsStringGrid: TStringGrid;
    GenresStringGrid: TStringGrid;
    PublishersStringGrid: TStringGrid;
    OperationsStringGrid: TStringGrid;
    BooksTab: TTabSheet;
    AuthorsTab: TTabSheet;
    GenresTab: TTabSheet;
    PublishersTab: TTabSheet;
    OperationsTab: TTabSheet;
    procedure btnAddClick(Sender: TObject);
    procedure btnAuthorsClick(Sender: TObject);
    procedure btnGenresClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DeleteRowsFromStringGrid(const StringGrid : TStringGrid);
    procedure GenresTabShow(Sender: TObject);
    procedure OperationsStringGridSelection(Sender: TObject; aCol, aRow: Integer
      );
    procedure OperationsTabShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PublishersTabShow(Sender: TObject);

    procedure ReadDataFromDB(WhatRead : DataType; const ObjectList : TObjectList);
    procedure BooksTabShow(Sender: TObject);
    procedure AuthorsTabShow(Sender: TObject);

    function CheckPublisher(PubName : String) : Integer;

    function Split (const Delimiter: Char; Input: String) : TStrings;

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MyBooks: TMyBooks;
  BooksList : TObjectList;
  AuthorsList : TObjectList;
  PublishersList : TObjectList;
  CompositionsList : TObjectList;
  GenresList : TObjectList;
  EditorsList : TObjectList;
  TranslatorsList : TObjectList;

implementation

{$R *.lfm}

{ TMyBooks }

function TMyBooks.Split (const Delimiter: Char; Input: String) : TStrings;
begin
     Result:=TStrings.Create;
     Result.StrictDelimiter := true;
     Result.Delimiter := Delimiter;
     Result.Add(Input);
end;

function TMyBooks.CheckPublisher(PubName : String) : Integer;
begin
     SQLQuery1.Close;
     SQLQuery1.SQL.Text:='SELECT id FROM publishers';
     SQLQuery1.Open;

     if SQLQuery1.RecordCount = 0 then
     begin
         Result:=-1;
         exit;
     end
     else
     begin
          Result := SQLQuery1.RecordCount;
     end;
end;

procedure TMyBooks.ReadDataFromDB(WhatRead : DataType; const ObjectList : TObjectList);
var
  ids : array of Integer;
  I : Integer;
begin
     SQLQuery1.Close;
     case WhatRead of
          Books: SQLQuery1.SQL.Text:='SELECT id FROM books';
          Compositions: SQLQuery1.SQL.Text:='SELECT id FROM compositions';
          Authors: SQLQuery1.SQL.Text:='SELECT id FROM authors';
          Editors: SQLQuery1.SQL.Text:='SELECT id FROM editors';
          Translators: SQLQuery1.SQL.Text:='SELECT id FROM translators';
          Genres: SQLQuery1.SQL.Text:='SELECT id FROM genres';
          Publishers: SQLQuery1.SQL.Text:='SELECT id FROM publishers';
     end;

     SQLQuery1.Open;

     if SQLQuery1.RecordCount = 0 then
        exit;
     SetLength(ids,SQLQuery1.RecordCount);
     I:=0;
     while not SQLQuery1.EOF do
     begin
          ids[I]:=SQLQuery1.FieldByName('id').AsInteger;
          SQLQuery1.Next;
          I:=I+1;
     end;
     SQLQuery1.Close;

     if WhatRead = Books then
     begin
          for I:=0 to Length(ids)-1 do
          begin
               ObjectList.Add(TMBBook.Create(ids[I], SQLQuery1, SQLTransaction1));
          end;
     end;

     if WhatRead = Compositions then
     begin
          for I:=0 to Length(ids)-1 do
          begin
               ObjectList.Add(TMBComposition.Create(ids[I], SQLQuery1, SQLTransaction1));
          end;
     end;

     if WhatRead = Authors then
     begin
          for I:=0 to Length(ids)-1 do
          begin
               ObjectList.Add(TMBAuthor.Create(ids[I], SQLQuery1, SQLTransaction1));
          end;
     end;

     if WhatRead = Editors then
     begin
          for I:=0 to Length(ids)-1 do
          begin
               ObjectList.Add(TMBEditor.Create(ids[I], SQLQuery1, SQLTransaction1));
          end;
     end;

     if WhatRead = Translators then
     begin
          for I:=0 to Length(ids)-1 do
          begin
               ObjectList.Add(TMBTranslator.Create(ids[I], SQLQuery1, SQLTransaction1));
          end;
     end;

     if WhatRead = Publishers then
     begin
          for I:=0 to Length(ids)-1 do
          begin
               ObjectList.Add(TMBPublisher.Create(ids[I], SQLQuery1, SQLTransaction1));
          end;
     end;

     if WhatRead = Genres then
     begin
          for I:=0 to Length(ids)-1 do
          begin
               ObjectList.Add(TMBGenre.Create(ids[I], SQLQuery1, SQLTransaction1));
          end;
     end;
end;

procedure TMyBooks.BooksTabShow(Sender: TObject);
var
  I : Integer;
  book : TMBBook;
begin
     DeleteRowsFromStringGrid(BooksStringGrid);

     //ДК, обработка запроса, пытаемся вывести результат

     for I:=0 to BooksList.Count-1 do
     begin
          book:=BooksList.Items[I] as TMBBook;
          BooksStringGrid.InsertRowWithValues(I+1, [IntToStr(book.BookID), book.BookName, book.BookISBN, book.BookName, book.BookYear, book.BookNote]);
     end;

end;

procedure TMyBooks.AuthorsTabShow(Sender: TObject);
var
  I : Integer;
  author : TMBAuthor;
begin
     DeleteRowsFromStringGrid(AuthorsStringGrid);

     //ДК, обработка запроса, пытаемся вывести результат

     for I:=0 to AuthorsList.Count-1 do
     begin
          author:=AuthorsList.Items[I] as TMBAuthor;
          AuthorsStringGrid.InsertRowWithValues(I+1, [IntToStr(author.AuthorID), author.AuthorName, author.AuthorSurname]);
     end;

end;


procedure TMyBooks.FormCreate(Sender: TObject);
var
  BookCount : Integer;
begin
  //ДК, получаем количество книг, создаем массив и саписываем туда значения
  SQLQuery1.Close;
  BooksList:=TObjectList.Create;
  CompositionsList:=TObjectList.Create;
  AuthorsList:=TObjectList.Create;
  EditorsList:=TObjectList.Create;
  TranslatorsList:=TObjectList.Create;
  GenresList:=TObjectList.Create;
  PublishersList:=TObjectList.Create;

  ReadDataFromDB(Books, BooksList);
  ReadDataFromDB(Compositions, CompositionsList);
  ReadDataFromDB(Authors, AuthorsList);
  ReadDataFromDB(Editors, EditorsList);
  ReadDataFromDB(Translators, TranslatorsList);
  ReadDataFromDB(Genres, GenresList);
  ReadDataFromDB(Publishers, PublishersList);

end;

//ДК, очищаем таблицу, оставляем только строку с заголовками
procedure TMyBooks.DeleteRowsFromStringGrid(const StringGrid : TStringGrid);
var
  I : Integer;
begin
     StringGrid.Clean([gzNormal]);

     if StringGrid.RowCount > 1 then
     begin
         for I:=StringGrid.RowCount downto 1 do;
              StringGrid.DeleteRow(I);
     end;
     StringGrid.RowCount:=1;

end;

procedure TMyBooks.GenresTabShow(Sender: TObject);
var
  I : Integer;
  genre : TMBGenre;
begin
     DeleteRowsFromStringGrid(GenresStringGrid);

     //ДК, обработка запроса, пытаемся вывести результат

     for I:=0 to GenresList.Count-1 do
     begin
          genre:=GenresList.Items[I] as TMBGenre;
          GenresStringGrid.InsertRowWithValues(I+1, [IntToStr(genre.GenreID), genre.GenreName, genre.GenreDescription]);
     end;
end;

procedure TMyBooks.OperationsStringGridSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
    //ShowMessage(Concat('Selected Ccell: col=', IntToStr(aCol), ' row=', IntToStr(aRow), ' call value=', OperationsStringGrid.Cells[aCol, aRow]));
end;

procedure TMyBooks.OperationsTabShow(Sender: TObject);
var
  I, J : Integer;
  publisher : TMBPublisher;
  genre : TMBGenre;
  author : TMBAuthor;
  editor : TMBEditor;
  translator : TMBTranslator;
begin
     //ДК, заполняем PickList для издателей
     if PublishersList.Count > 0 then
     begin
         for I:=0 to PublishersList.Count -1 do
         begin
              publisher:=PublishersList.Items[I] as TMBPublisher;
              OperationsStringGrid.Columns[8].PickList.Add(publisher.PublisherName);
         end;
     end;

     //ДК, заполняем PickList для жанров
     if GenresList.Count > 0 then
     begin
          for I:=0 to GenresList.Count do
          begin
               genre:=GenresList.Items[I] as TMBGenre;
               OperationsStringGrid.Columns[5].PickList.Add(genre.GenreName);
          end;
     end;

     //ДК, заполняем PickList для Авторов
     if AuthorsList.Count > 0 then
     begin
          for I:=0 to AuthorsList.Count do
          begin
               author:=AuthorsList.Items[I] as TMBAuthor;
               OperationsStringGrid.Columns[3].PickList.Add(Concat(author.AuthorSurname, ' ', author.AuthorName));
          end;
     end;

     //ДК, заподняем PickList для переводчиков
     if TranslatorsList.Count > 0 then
     begin
          for I:=0 to TranslatorsList.Count do
          begin
               translator:=TranslatorsList.Items[I] as TMBTranslator;
               OperationsStringGrid.Columns[7].PickList.Add(Concat(translator.TranslatorSurname, ' ', translator.TranslatorName));
          end;
     end;

     //ДК, заполняем PickList для редакторов
     if EditorsList.Count > 0 then
     begin
          for I:=0 to EditorsList.Count do
          begin
               editor:=EditorsList.Items[I] as TMBEditor;
               OperationsStringGrid.Columns[6].PickList.Add(Concat(editor.EditorSurname, ' ', editor.EditorName));
          end;
     end;

end;

procedure TMyBooks.PageControl1Change(Sender: TObject);
begin

end;

procedure TMyBooks.PublishersTabShow(Sender: TObject);
var
  I : Integer;
  publisher : TMBPublisher;
begin
     DeleteRowsFromStringGrid(PublishersStringGrid);

     //ДК, обработка запроса, пытаемся вывести результат
     if PublishersList.Count > 0 then
     begin
          for I:=0 to PublishersList.Count-1 do
          begin
               publisher:=PublishersList.Items[I] as TMBPublisher;
               PublishersStringGrid.InsertRowWithValues(I+1, [IntToStr(publisher.PublisherID), publisher.PublisherName, publisher.PublisherURL, publisher.PublisherCity]);
          end;
     end;

end;

procedure TMyBooks.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     SQLQuery1.Close;
     SQLTransaction1.Active:=False;
     SQLite3Connection1.Connected:=False;
end;

procedure TMyBooks.btnAuthorsClick(Sender: TObject);
begin

end;

procedure TMyBooks.btnAddClick(Sender: TObject);
var
  I, J : Integer;
  str : String;
  FIO : TStringList;
  newBook : TMBBook;
  newAuthor : TMBAuthor;
  newPublisher : TMBPublisher;
  newComposition : TMBComposition;
  newGenre : TMBGenre;
  newEditor : TMBEditor;
  newTranslator : TMBTranslator;
begin
     //ДК, обрабатываем строки, алгоритм будет следующий:
     //1 - обрабатываем колонки по отдельности, каждой колонке
     //по одновому объекту, для объекта создается запись в БД,
     //если ее не было, либо данные загружаются из БД в объект
     //2 - связываем автора и произведение
     //3 - свзываем книгу и жанр
     //4 - связываем книгу и издателя
     //5 - связываем книгу и редактора
     //6 - связываем книгу и переводчика
     //8 - связываем книгу и произведение
     FIO:=TStringList.Create;
     FIO.StrictDelimiter := true;
     FIO.Delimiter := ' ';

     for I:=1 to OperationsStringGrid.RowCount -1 do
     begin
          //ДК, издатель
          if OperationsStringGrid.Cells[9,I] <> '' then
          begin
               newPublisher.Create(OperationsStringGrid.Cells[9,I], SQLQuery1, SQLTransaction1);
          end;
          //ДК, переводчик
          if OperationsStringGrid.Cells[8,I] <> '' then
          begin
               FIO.DelimitedText:=OperationsStringGrid.Cells[8,I];
               //FIO:=Split(' ', OperationsStringGrid.Cells[8,I]);
               ShowMessage(IntToStr(FIO.Count));
               if FIO.Count = 1 then
               begin
                    newTranslator.Create('', FIO[0], SQLQuery1, SQLTransaction1);
               end
               else
               begin
                   newTranslator.Create(FIO[1], FIO[0], SQLQuery1, SQLTransaction1);
               end;
               FIO.Clear;
          end;

          //ДК, редактор
          if OperationsStringGrid.Cells[6,I] <> '' then
          begin
               FIO.DelimitedText:=OperationsStringGrid.Cells[6,I];
               //FIO:=Split(' ', OperationsStringGrid.Cells[6,I]);
               if FIO.Count = 1 then
               begin
                    newEditor.Create('', FIO[0], SQLQuery1, SQLTransaction1);
               end
               else
               begin
                   newEditor.Create(FIO[1], FIO[0], SQLQuery1, SQLTransaction1);
               end;
               FIO.Clear;
          end;

          if OperationsStringGrid.Cells[5,I] <> '' then
          begin
               newGenre.Create(OperationsStringGrid.Cells[9,I], '', SQLQuery1, SQLTransaction1);
          end;

          if OperationsStringGrid.Cells[3,I] <> '' then
          begin
               FIO.DelimitedText:=OperationsStringGrid.Cells[6,I];
               //FIO:=Split(' ', OperationsStringGrid.Cells[6,I]);
               //ДК, здесь наверняка нудна какая-нибудь проверка
               newAuthor.Create(FIO[1], FIO[0], SQLQuery1, SQLTransaction1);
               FIO.Clear;
          end;

          if OperationsStringGrid.Cells[4,I] <> '' then
          begin
               newComposition.Create(OperationsStringGrid.Cells[4,I], '', SQLQuery1, SQLTransaction1);
          end;

          newBook.Create(OperationsStringGrid.Cells[0,I], OperationsStringGrid.Cells[1,I], OperationsStringGrid.Cells[2,I], '', SQLQuery1, SQLTransaction1);

          newAuthor.AddComposition(newComposition.CompositionID,SQLQuery1, SQLTransaction1);
          newBook.AddGenre(newGenre.GenreID,SQLQuery1, SQLTransaction1);
          newBook.AddPublisher(newPublisher.PublisherID,SQLQuery1, SQLTransaction1);
          newBook.AddEditor(newEditor.EditorID,SQLQuery1, SQLTransaction1);
          newBook.AddComposition(newComposition.CompositionID,SQLQuery1, SQLTransaction1);
     end;

end;

procedure TMyBooks.btnGenresClick(Sender: TObject);
begin

end;

//procedure TMyBooks.dtnAddBookClick(Sender: TObject);
//begin
//     SQLQuery1.Close;
//     SQLQuery1.SQL.Text:='INSERT INTO books (name, isbn, orig_name, year, note) VALUES (:bNAME, :bISBN, :bORIGNAME, :bYEAR, :bNOTE)';
//     SQLQuery1.Params.ParamByName('bNAME').AsString:=edtBookName.Text;
//     SQLQuery1.Params.ParamByName('bISBN').AsString:=edtBookISBN.Text;
//     SQLQuery1.Params.ParamByName('bORIGNAME').AsString:=edtOrigName.Text;
//     SQLQuery1.Params.ParamByName('bYEAR').AsString:=edtBookYear.Text;
//     SQLQuery1.Params.ParamByName('bNOTE').AsString:=edtBookAnnotation.Text;
//     SQLQuery1.ExecSQL;
//     SQLTransaction1.Commit;
//end;


end.

