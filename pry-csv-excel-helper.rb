require 'csv'
require 'caxlsx'
require 'roo'
require 'active_support/core_ext/hash'

CSV_BOM = "\xef\xbb\xbf"

module Kernel
  def generate_csv(filename, **options, &block)
    opts = {
      col_sep: "\t",
      row_sep: "\r\n"
    }
    opts.merge!(options.except(:encoding))
    encoding = options[:encoding] || 'UTF-16LE'
    File.open(File.expand_path(filename), "w:#{encoding}") do |file|
      file.write(CSV_BOM)
      file.write CSV.generate(**opts, &block)
    end
  end

  def parse_csv(filename, **options)
    encoding = options[:encoding] || 'UTF-16'
    opts = {
      headers: false,
      col_sep: "\t",
      row_sep: "\r\n"
    }
    opts.merge!(options.except(:encoding))
    CSV.parse(IO.read(File.expand_path(filename), encoding: encoding, binmode: true).encode('UTF-8'), **opts).to_a
  end

  def generate_excel(filename)
    Axlsx::Package.new do |package|
      yield(package.workbook)
      package.serialize(filename)
    end
  end

  def parse_excel(filename)
    xlsx = Roo::Excelx.new(File.expand_path(filename))
    xlsx.sheets.each_with_object({}) do |sheet_name, result|
      begin
        result[sheet_name] = xlsx.sheet(sheet_name).to_a
      rescue
      end
    end
  end
end

class Array
  def write_csv(filename, **options)
    generate_csv(filename, **options) do |csv|
      each { |row| csv << row }
    end
  end

  def write_excel(filename, sheet_name = 'Sheet1')
    generate_excel(filename) do |workbook|
      workbook.add_worksheet(name: sheet_name) do |sheet|
        each { |row| sheet.add_row(row, types: [:string] * row.size) }
      end
    end
  end
end

class Hash
  def write_excel(filename)
    generate_excel(filename) do |workbook|
      each do |sheet_name, sheet_data|
        workbook.add_worksheet(name: sheet_name) do |sheet|
          sheet_data.each { |row| sheet.add_row(row, types: [:string] * row.size) }
        end
      end
    end
  end
end
