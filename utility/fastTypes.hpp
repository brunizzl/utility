#pragma once

#include <cstddef>
#include <cassert>
#include <bit>
#include <type_traits>
#include <concepts>


namespace simp {

	enum class PtrState { pointer = 0, integer, COUNT };

	template<typename PointedAt, typename TagEnum = PtrState, std::integral Int_T = std::uintptr_t>
	class PtrOrInt
	{
		static constexpr std::size_t needed_tag_bits = std::bit_width((std::size_t)TagEnum::COUNT - 1);

		PointedAt* ptr;

		//has to return zero iff this currently represents a pointer
		constexpr std::uintptr_t tag_bits() const noexcept
		{
			//check if enum fits in unused pointer pits
			//(only done here, because otherwise PointedAt would need to be defined, when PtrOrInt is instatiated)
			constexpr std::size_t nr_unused_lower_ptr_bits = std::bit_width(alignof(PointedAt)) - 1;
			static_assert(nr_unused_lower_ptr_bits >= needed_tag_bits);

			if (std::is_constant_evaluated()) {
				//at compile time only the pointer alternative can be held, as std::bit_cast (the one used here anyway) is forbidden.
				//i hate this as much as you. trust me.
				return 0;
			}
			constexpr std::uintptr_t tag_mask = (1ull << needed_tag_bits) - 1;
			return std::bit_cast<std::uintptr_t>(this->ptr) & tag_mask;
		} //tag_bits

		template<typename, typename, std::integral>
		friend class PtrOrInt;

	public:
		constexpr PtrOrInt(const PtrOrInt&) noexcept = default;

		template<typename SndInt_T> requires (std::is_const_v<PointedAt>) //requirement stops redeclaration of ctor above
		constexpr PtrOrInt(const PtrOrInt<std::remove_const_t<PointedAt>, TagEnum, SndInt_T>& snd) noexcept :ptr(snd.ptr) {}

		constexpr PtrOrInt(PointedAt* const new_ptr) noexcept :ptr(new_ptr) {}

		PtrOrInt(Int_T const val, TagEnum const tag) noexcept
			:ptr(std::bit_cast<PointedAt*>(((std::uintptr_t)val << needed_tag_bits) | (std::uintptr_t)tag))
		{
			assert(tag > static_cast<TagEnum>(0) && tag < TagEnum::COUNT); //zero represents pointer case
		}

		PtrOrInt(Int_T const val) noexcept requires (std::is_same_v<TagEnum, PtrState>)
			: ptr(std::bit_cast<PointedAt*>(((std::uintptr_t)val << needed_tag_bits) | (std::uintptr_t)PtrState::integer))
		{}

		constexpr PointedAt& operator*() const noexcept { assert(!this->tag_bits()); return *this->ptr; }
		constexpr PointedAt* operator->() const noexcept { assert(!this->tag_bits()); return this->ptr; }

		Int_T get_int() const noexcept
		{
			assert(this->tag_bits());

			using Intermediate = std::conditional_t<std::is_signed_v<Int_T>, std::intptr_t, std::uintptr_t>;
			const auto shifted = std::bit_cast<Intermediate>(this->ptr) >> needed_tag_bits; //respect sign when shifting
			return static_cast<Int_T>(shifted); //only cast to (possibly smaller type) after shifting
		}

		constexpr TagEnum tag() const noexcept { return static_cast<TagEnum>(this->tag_bits()); }

	}; //struct PtrOrInt

} //namespace simp