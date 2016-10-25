#include <iostream.h>
#include <fstream.h>
#include <string.h>

typedef unsigned __int8 byte;
typedef unsigned __int16 word;

word checksum;
ofstream out_file;

void write_num(word num_to_write);
void write_str(char * str_to_write, int size, bool zero_pad = false);

word lite86_compress(byte * data_out, byte * data_in, word data_in_len);
word lite86_decompress(byte * data_out, byte * data_in);

#pragma pack(push,1)			// prevent these structures from being padded

struct megacar86_level
{
	byte level_id[2];			// 'M',1
	word bestlap;
	byte bestlap_initials[4];
	word bestrace;
	byte bestrace_initials[4];	// part 1 = 14 bytes

	byte width;
	byte height;
	byte angle;
	byte car_x_fraction;
	word car_x;
	byte car_y_fraction;
	word car_y;
	byte laps;
	word posts[16];				// part 2 = 42 bytes

	byte level_data[65536];
};

struct megacar83p_level
{
	byte detect_str[4];			// "MgC1"
	word bestlap;
	byte bestlap_initials[4];
	word bestrace;
	byte bestrace_initials[4];
	byte width;
	byte height;
	byte angle;
	byte laps;
	word car_x;
	word car_y;
	byte posts[16];				// 40 bytes

	byte level_data[65536];
};

#pragma pack(pop)
void main(int argc, char * argv[])
{
	word data_len;
	char file_name[9];
	char file_name_ext[13];
	int i;

	const char detect_str[] = "MgC1";

	char char_buf[65536];
	byte byte_buf1[65536];
	byte byte_buf2[65536];
	
	megacar86_level old_level;
	megacar83p_level new_level;

	cout << "Megacar 86 -> 83+ Level Converter (MC83PLC) v1.0\n";
	
	if (argc < 2)
	{
		cout << "\nEnter name of .86S file to convert: ";
		cin.getline(char_buf,256);
	}
	else
	{
		strcpy(char_buf,argv[1]);
	}

	
	i = -1;
	do
	{
		i++;
		if (char_buf[i] == '.') char_buf[i] = 0;
	}
	while (char_buf[i] != 0);
	
	if ((strlen(char_buf) < 1) || (strlen(char_buf) > 8))
	{
		cout << "Error: File name must be 1 to 8 characters\n";
		return;
	}

	strcpy(file_name, char_buf);
	
	for (i=0; i<9; i++)
		if ((file_name[i] >= 'a') && (file_name[i] <= 'z')) file_name[i] += ('A'-'a');

	strcpy(file_name_ext, file_name);
	strcat(file_name_ext, ".86S");

	ifstream in_file;
	in_file.open(file_name_ext, ios::in | ios::binary | ios::nocreate);

	if (in_file == NULL)
	{
		cout << "Error: Unable to open input file: " << file_name_ext << '\n';
		return;
	}
	
	in_file.seekg(0x47,ios::beg);
	in_file.read(char_buf, 2);
	memcpy(&data_len, char_buf, 2);
	
	in_file.read(char_buf, data_len);
	in_file.close();
	
	memcpy(&old_level, char_buf, 14);		// copy uncompressed part to the structure
	
	memcpy(byte_buf1, char_buf + 14, data_len - 14);
	data_len = lite86_decompress(byte_buf2, byte_buf1);	// decompress compressed part
	memcpy(&old_level.width, byte_buf2, data_len);		// and copy it to to .width

	memcpy(&new_level.detect_str, detect_str, 4);
	memcpy(&new_level.bestlap, &old_level.bestlap, 15);	// copy bestlap, bestrace, width, height, angle
	new_level.laps = old_level.laps;					// copy lap count
	new_level.car_x = old_level.car_x;
	new_level.car_y = old_level.car_y;
	for (i=0; i<16; i++) new_level.posts[i] = old_level.posts[i] / 8;
	
	data_len = 40 + lite86_compress(new_level.level_data, old_level.level_data, data_len - 42);

	memcpy(char_buf, &new_level, data_len);				// copy header+data to char_buf

	strcpy(file_name_ext, file_name);
	strcat(file_name_ext, ".8XP");
	out_file.open(file_name_ext, ios::out | ios::binary | ios::trunc);
	
	if (out_file == NULL)
	{
		cout << "Error: Unable to create output file: " << file_name_ext << '\n';
		return;
	}

	write_str("**TI83F*\x1A\x0A\x00",11);
	write_str("Megacar 86->83+ Level Converter", 42, true);
	write_num(data_len + 19);

	// Data Section
	checksum = 0;
	write_str("\x0D\x00",2);
	write_num(data_len + 2);
	write_str("\x06",1);
	write_str(file_name, 10, true);
	write_num(data_len + 2);
	write_num(data_len);
	write_str(char_buf, data_len);
	write_num(checksum);
	
	out_file.close();

	cout << file_name_ext << " created successfully\n";
	cout << "Program size: " << (data_len + strlen(file_name) + 9) << " bytes\n";
}


word lite86_compress(byte * data_out, byte * data_in, word data_in_len)
{
	// This is a modified version of Kirk Meyer's Lite86 II routine

	word in_pos, match_pos, match_len;
	word max_match_pos, max_match_len;
	
	byte header_mask = 0x80;

	byte block_buf[256];
	byte block_buf_len = 1;

	word data_out_len = 0;
	word offset_val;

	block_buf[0] = 0;
	for (in_pos = 0; in_pos < data_in_len; in_pos++)
	{
		for (max_match_len = 0, match_pos = (in_pos < 2048 ? 0 : in_pos - 2048); match_pos < in_pos; match_pos++)
		{
			for (match_len = in_pos; match_len < (in_pos + 34 >= data_in_len ? data_in_len : in_pos + 34); match_len++)
				if (data_in[match_len] != data_in[match_pos + match_len - in_pos]) break;

			match_len -= in_pos;

			if (match_len == 34 && (in_pos - match_pos) >= 1792) match_len--;	// we don't want a 0x1F in there!

			if (match_len >= max_match_len)
			{
				max_match_len = match_len;
				max_match_pos = match_pos;
			}
		}					

		if (max_match_len < 3)
		{
			header_mask >>= 1;
			block_buf[block_buf_len++] = data_in[in_pos];
			
			if (!header_mask)
			{
				memcpy(data_out + data_out_len, block_buf, block_buf_len);
				data_out_len += block_buf_len;
				block_buf[0] = 0;
				block_buf_len = 1;
				header_mask = 0x80;
			}
		}
		else
		{
			block_buf[0] |= header_mask;
			header_mask >>= 1;
			offset_val = max_match_pos - in_pos;
			block_buf[block_buf_len++] = (((offset_val & 0x700) >> 3) | (max_match_len - 3));
			block_buf[block_buf_len++] = (offset_val & 0xFF);
			
			in_pos += (max_match_len - 1);

			if (!header_mask)
			{
				memcpy(data_out + data_out_len, block_buf, block_buf_len);
				data_out_len += block_buf_len;
				block_buf[0] = 0;
				block_buf_len = 1;
				header_mask = 0x80;
			}
		}
	}
	block_buf[0] |= header_mask;
	block_buf[block_buf_len++] = 0x1F;

	memcpy(data_out + data_out_len, block_buf, block_buf_len);
	data_out_len += block_buf_len;

	return data_out_len;
}

word lite86_decompress(byte * data_out, byte * data_in)
{
	word data_in_pos = 0;
	word data_out_len = 0;
	word offset_val;
	byte block_len;
	byte bit_num;
	byte header_byte;
	byte data_byte;

	while (true)
	{
		header_byte = data_in[data_in_pos++];

		for (bit_num = 0; bit_num < 8; bit_num++)
		{
			data_byte = data_in[data_in_pos++];
			if (header_byte & 0x80)
			{
				if (data_byte == 0x1F) return data_out_len;
				
				offset_val = 65536 - (data_byte << 3 & 0x700 | 0xF800 | data_in[data_in_pos++]);
				block_len = (data_byte & 0x1F) + 3;
				
				while (block_len > 0)
				{
					data_out[data_out_len] = data_out[data_out_len - offset_val];
					data_out_len++;
					block_len--;
				}
			}
			else
			{
				data_out[data_out_len++] = data_byte;
			}
			
			header_byte <<= 1;
		}
	}
}

void write_num(word num_to_write)
{
	char str_to_write[2];
	memcpy(str_to_write, &num_to_write, 2);
	write_str(str_to_write, 2);
}


void write_str(char * str_to_write, int size, bool zero_pad)
{
	int i;
	int str_len;
	char byte_to_write;

	if (zero_pad) str_len = strlen(str_to_write);

	for (i=0; i < size; i++)
	{
		byte_to_write = (zero_pad && i > str_len) ? 0 : str_to_write[i];
		checksum += ((word) byte_to_write & 0xFF);
		out_file.put(byte_to_write);
	}
}